;;; org-srs-algorithm-fsrs-optimizer.el --- Integration of Python module `fsrs-optimizer' -*- lexical-binding:t -*-

;; Copyright (C) 2024-2025 Bohong Huang

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package can optimize parameters for a specific set of review
;; items that use the FSRS algorithm to improve review efficiency and
;; effectiveness.

;;; Code:

(require 'cl-lib)
(require 'json)

(require 'org-srs-query)
(require 'org-srs-item)
(require 'org-srs-table)
(require 'org-srs-time)

(require 'fsrs)

(cl-defun org-srs-algorithm-fsrs-optimizer-insert-review-log (markers buffer)
  (cl-loop initially (with-current-buffer buffer
                       (insert "review_time,card_id,review_rating,review_state")
                       (newline))
           for marker in markers
           for id from 0
           do (with-current-buffer (marker-buffer marker)
                (cl-loop initially (goto-char marker) (org-srs-table-goto-starred-line) (forward-line -1)
                         for timestamp = (org-srs-table-field 'timestamp)
                         for rating = (org-srs-table-field 'rating)
                         for state = (progn (forward-line -1) (org-srs-table-field 'state))
                         until (string-empty-p rating)
                         do (with-current-buffer buffer
                              (prin1 (truncate (* (time-to-seconds (org-srs-timestamp-time timestamp)) 1000)) buffer)
                              (insert ",")
                              (prin1 id buffer)
                              (insert ",")
                              (prin1 (fsrs-rating-integer (read rating)) buffer)
                              (insert ",")
                              (prin1 (fsrs-state-integer (read state)) buffer)
                              (newline))
                         until (org-at-table-hline-p)))))

(defun org-srs-algorithm-fsrs-optimizer-iana-tz ()
  (let ((output (shell-command-to-string "timedatectl")))
    (cl-assert (string-match (rx "Time zone:" (* blank) (group (+? anychar)) (* blank) "(") output))
    (match-string 1 output)))

(cl-defun org-srs-algorithm-fsrs-optimizer-start-process (file &optional (callback #'ignore))
  (let ((buffer (generate-new-buffer "*fsrs-optimizer*")))
    (cl-assert (null (get-buffer-process buffer)))
    (let ((process (let ((default-directory (temporary-file-directory)))
                     (start-process "fsrs-optimizer" buffer "python3" "-m" "fsrs_optimizer" (expand-file-name file))))
          (start-of-day (cl-loop for (amount unit) on (org-srs-time-start-of-next-day) by #'cddr
                                 sum (cl-ecase unit (:hour amount) (:sec (/ amount 60.0))))))
      (message "Optimizing...")
      (set-process-filter
       process
       (lambda (process prompt)
         (with-current-buffer buffer (insert prompt))
         (cl-block process-prompt
           (cond
            ((string-match-p (rx "input used timezone" (*? anychar) ":") prompt)
             (process-send-string process (org-srs-algorithm-fsrs-optimizer-iana-tz)))
            ((string-match-p (rx "input used next day start hour" (*? anychar) ":") prompt)
             (process-send-string process (prin1-to-string start-of-day)))
            ((string-match-p (rx "input the date at which before reviews will be ignored" (*? anychar) ":") prompt))
            ((string-match-p (rx "input filter out suspended cards" (*? anychar) ":") prompt)
             (process-send-string process "n"))
            ((string-match-p (rx "input enable short-term component in FSRS model?" (*? anychar) ":") prompt)
             (process-send-string process "y"))
            ((string-match-p (rx "Save graphs?" (*? anychar) ":") prompt)
             (process-send-string process "n"))
            (t (cl-return-from process-prompt)))
           (process-send-string process "\n"))))
      (set-process-sentinel
       process
       (lambda (_process _event)
         (cl-assert
          (zerop (process-exit-status process)) nil
          "Missing Python module `fsrs-optimizer'")
         (with-current-buffer buffer
           (goto-char (point-min))
           (cl-assert
            (re-search-forward (rx "Paste this into your scheduling code") nil t) nil
            "Insufficient review history or unknown optimizer output")
           (replace-regexp-in-region (rx "//" (*? anychar) eol) "" (point))
           (replace-regexp-in-region (rx "," (*? (char blank control)) "}") "}" (point))
           (funcall callback (cl-loop for (key . value) in (json-read)
                                      for keyword = (cl-case key
                                                      (w :weights)
                                                      (requestRetention :request-retention)
                                                      (maximumInterval :maximum-interval))
                                      when keyword nconc (list keyword value))))
         (kill-buffer buffer))))))

(cl-defun org-srs-algorithm-fsrs-optimizer-optimize (markers &optional (callback #'ignore))
  (let ((file (make-temp-file "org-srs-algorithm-fsrs-optimizer" nil ".csv")))
    (with-temp-buffer
      (let ((buffer (current-buffer)))
        (with-current-buffer (window-buffer)
          (save-window-excursion
            (org-srs-algorithm-fsrs-optimizer-insert-review-log markers buffer))))
      (append-to-file (point-min) (point-max) file))
    (org-srs-algorithm-fsrs-optimizer-start-process
     file
     (lambda (&rest args)
       (delete-file file)
       (apply callback args)))))

;;;###autoload
(defun org-srs-algorithm-fsrs-optimize (source)
  "Optimize the review items in SOURCE.

If called interactively, perform the optimization in the current
file or directory.
If called interactively with a `\\[universal-argument]` prefix,
prompt the user to select the scope of items for optimization."
  (interactive
   (list (cl-destructuring-bind (&optional (arg 1)) current-prefix-arg
           (if (> arg 1)
               (read-file-name "File or directory for optimization: " nil default-directory t)
             (or (buffer-file-name (current-buffer)) default-directory)))))
  (cl-check-type source string)
  (let ((file source))
    (org-srs-algorithm-fsrs-optimizer-optimize
     (let ((markers nil))
       (message "Collecting review history for optimization...")
       (org-srs-query `(and (not new) ,(lambda () (push (point-marker) markers))) file)
       markers)
     (let ((apply-local-variable-function
            (if (file-directory-p file)
                (lambda (parameters)
                  (let ((default-directory file))
                    (add-dir-local-variable 'org-mode 'org-srs-algorithm (cons 'fsrs parameters))))
              (lambda (parameters)
                (with-current-buffer (find-file-noselect file)
                  (save-excursion
                    (if-let ((position (org-find-property "SRS_ALGORITHM")))
                        (progn
                          (goto-char position)
                          (org-set-property "SRS_ALGORITHM" (prin1-to-string (cons 'fsrs parameters))))
                      (add-file-local-variable 'org-srs-algorithm (cons 'fsrs parameters)))))))))
       (lambda (&rest args)
         (save-window-excursion (apply apply-local-variable-function args))
         (message "Optimization finished"))))))

(provide 'org-srs-algorithm-fsrs-optimizer)
;;; org-srs-algorithm-fsrs-optimizer.el ends here
