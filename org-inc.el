;;; org-inc.el --- FSRS algorithm integration for Org-srs -*- lexical-binding: t; -*-

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

;; This package serves as the algorithm interface for Org-srs, allowing
;; the core to be independent of specific algorithms.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)

(require 'org-srs-algorithm)
(require 'org-srs-time)
(require 'org-srs-embed)
(require 'org-srs-schedule-step)
(require 'org-srs-review-cache)

(cl-defmethod org-srs-algorithm-repeat ((_ (eql 'nontopic)) args)
  (cl-assert (not (eq org-srs-algorithm 'nontopic)))
  (org-srs-algorithm-repeat
   (org-srs-property-let ((org-srs-algorithm org-srs-algorithm))
     (org-srs-algorithm-ensure org-srs-algorithm))
   args))

(cl-defun org-inc-topic-a-factor ()
  (org-srs-item-with-current ('topic)
    (cl-loop initially (org-srs-table-goto-starred-line)
             for a-factor = (org-srs-table-ensure-read-field (org-srs-table-field 'a_factor))
             when a-factor return a-factor
             do (forward-line -1)
             until (org-at-table-hline-p))))

(defun org-inc-a-factor-parent (x)
  (let ((a 0.05) (b 0.95))
    (+ a (* b x))))

(defun org-inc-a-factor-child (x)
  (let ((a 2.65204663673051)
        (b -2.08925219374674)
        (c 4.1555228033838e-07)
        (d -31401387039204.6))
    (+ (/ (- a d) (1+ (expt (/ x c) b))) d)))

(defun org-inc-a-factor-review (x)
  (let ((a -0.20) (b 1.18))
    (+ a (* b x))))

(cl-defun org-inc-a-factor-topic-initial-interval (&optional (a-factor 0.0))
  (let ((a 0.77) (b 2.9))
    (round (+ a (* b a-factor)))))

(defvar org-inc-a-factor 1.2)

(cl-defun org-inc-a-factor (&optional (x org-inc-a-factor))
  (min (max x 1.2) 6.9))

(cl-deftype org-inc-action ()
  '(member :create :extract :derive :review :postpone))

(defvar org-inc-action)

(cl-defmethod org-srs-algorithm-repeat ((_ (eql 'topic)) (_args null))
  `((a_factor . ,org-inc-a-factor) (action . ,(or (bound-and-true-p org-inc-action) :create))))

(cl-defun org-inc-last-review-timestamp ()
  (save-excursion
    (cl-assert (org-at-table-p))
    (cl-loop initially (org-srs-table-goto-starred-line)
             for action = (org-srs-table-ensure-read-field (org-srs-table-field 'action))
             when (member action '(:create :review :derive))
             return (org-srs-table-field 'timestamp)
             do (forward-line -1)
             until (org-at-table-hline-p))))

(cl-defmethod org-srs-algorithm-repeat ((_ (eql 'topic)) (args list))
  (cl-destructuring-bind (review-timestamp due-timestamp last-review-timestamp)
      (cl-loop for (name . value) in args when (eq name 'timestamp) collect value)
    (setf last-review-timestamp (org-inc-last-review-timestamp))
    (cl-assert (org-srs-timestamp< last-review-timestamp due-timestamp))
    (let ((a-factor (alist-get 'a_factor args)))
      `((timestamp . ,(org-srs-timestamp+
                       review-timestamp
                       (ceiling
                        (* (org-srs-timestamp-difference due-timestamp last-review-timestamp) a-factor)
                        (* 60 60 24))
                       :day))
        (a_factor . ,(org-inc-a-factor-review a-factor))
        (action . :review)))))

(defvar org-inc-continue)

(cl-defmethod org-srs-item-review ((type (eql 'topic)) &rest args)
  (let ((entry-beginning (copy-marker (org-entry-beginning-position)))
        (entry-end (copy-marker (1- (org-entry-end-position)))))
    (org-srs-item-narrow)
    (org-srs-item-add-hook-once
     'org-srs-item-after-confirm-hook
     (lambda ()
       (when (org-srs-reviewing-p)
         (if (<= entry-beginning (point) entry-end)
             (when (and (or (not (boundp 'org-inc-continue))
                            (symbol-value 'org-inc-continue))
                        (not (boundp 'org-srs-reviewing-p)))
               (org-srs-review-rate nil))
           (goto-char entry-beginning)
           (apply #'org-srs-item-review type args))))
     90)
    (apply (org-srs-item-confirm) type args)))

(cl-defmethod org-srs-review-strategy-items ((type (eql 'todo)) (_strategy (eql 'rotate)) &rest args)
  (cl-loop with total-length = (mod (length (apply #'org-srs-review-strategy-items 'done 'rotate args))
                                    (cl-reduce #'+ args :key #'car))
           for (times . strategy) in args
           sum times into length
           when (> length total-length)
           do (cl-block continue (cl-return (or (org-srs-review-strategy-items type strategy) (cl-return-from continue))))))

(defun org-srs-query-predicate-topic ()
  (lambda () (eq (car (cl-nth-value 0 (org-srs-item-at-point))) 'topic)))

(defun org-inc-f64-u64le (f64)
  (cl-assert (not (isnan f64)))
  (cl-destructuring-bind (fraction . exponent) (frexp (abs f64))
    (cl-assert (<= fraction 1.0))
    (let ((minusp (cl-minusp f64))
          (biased-exponent 0)
          (mantissa 0))
      (unless (zerop fraction)
        (setf biased-exponent (+ exponent 1022)
              mantissa (truncate (ldexp fraction 53))))
      (let* ((sign-bit (if minusp #x8000000000000000 0))
             (exponent (ash (logand biased-exponent #x7FF) 52))
             (mantissa (logand mantissa (1- (ash 1 52)))))
        (logior sign-bit exponent mantissa)))))

(defun org-inc-u64le-f64 (u64le)
  (let* ((msb0 (logand (ash u64le -56) #xFF))
         (msb1 (logand (ash u64le -48) #xFF))
         (minusp (= #x80 (logand msb0 #x80)))
         (exponent (logand (logior (ash msb0 4) (ash msb1 -4)) #x7FF))
         (mantissa (logior #x10000000000000 (logand u64le #xFFFFFFFFFFFFF)))
         (result (cond
                  ((/= #x7FF exponent)
                   (ldexp (ldexp mantissa -53) (- exponent 1022)))
                  ((= #x10000000000000 mantissa) 1.0e+INF)
                  (t 0.0e+NaN))))
    (if minusp (- result) result)))

(defconst org-inc-priority-min (org-inc-f64-u64le 0.0))
(defconst org-inc-priority-max (org-inc-f64-u64le 1.0))

(defun org-inc-priority-between (a b)
  (org-inc-f64-u64le (/ (+ (org-inc-u64le-f64 a) (org-inc-u64le-f64 b)) 2.0)))

(defun org-inc-priority-default ()
  (if-let ((priorities (org-inc-priorities)))
      (org-inc-priority-between org-inc-priority-min (car (cl-first (cl-sort priorities #'< :key #'car))))
    (org-inc-f64-u64le 0.5)))

(defvar org-inc-priority-default #'org-inc-priority-default)

(defun org-inc-priority-current ()
  (save-excursion
    (org-back-to-heading)
    (when (re-search-forward org-priority-regexp (line-end-position) t)
      (string-to-number (match-string 2)))))

(defun \(setf\ org-inc-priority-current\) (value)
  (save-excursion
    (org-back-to-heading)
    (if (re-search-forward org-priority-regexp (line-end-position) t)
        (if value
            (replace-match (prin1-to-string value) t t nil 2)
          (replace-match "" t t nil 1))
      (cl-assert (looking-at org-complex-heading-regexp))
      (goto-char (or (match-beginning 4) (match-end 3) (match-end 2) (match-end 1)))
      (unless (match-beginning 4) (forward-char 1))
      (insert (format "[#%d] " value)))))

(cl-defun org-inc-priority-previous (&optional (current (org-inc-priority-current)))
  (save-restriction
    (widen)
    (save-excursion
      (cl-loop initially (goto-char (point-min))
               for priority = org-inc-priority-min then (string-to-number (match-string-no-properties 2))
               when (< priority current)
               maximize priority
               while (re-search-forward org-priority-regexp nil t)))))

(cl-defun org-inc-priority-next (&optional (current (org-inc-priority-current)))
  (save-restriction
    (widen)
    (save-excursion
      (cl-loop initially (goto-char (point-min))
               for priority = org-inc-priority-max then (string-to-number (match-string-no-properties 2))
               when (> priority current)
               minimize priority
               while (re-search-forward org-priority-regexp nil t)))))

(cl-defmethod org-srs-item-new ((_type (eql 'topic)) &rest _args)
  (org-srs-property-let ((org-srs-algorithm 'topic)
                         (org-srs-item-confirm #'org-inc-continue))
    (cl-call-next-method)
    (setf (org-srs-property-plist) (list :algorithm (org-srs-algorithm) :item-confirm (org-srs-item-confirm))
          (org-srs-item-due-timestamp) (org-srs-timestamp+ (org-srs-item-due-timestamp) (org-inc-a-factor-topic-initial-interval) :day)
          (org-inc-priority-current) org-inc-priority-max)
    (org-inc-priority-set 0.0)))

(cl-defmethod org-srs-review-strategy-items (_type (_strategy (eql 'topic)) &rest _args)
  (defvar org-srs-review-source)
  (org-srs-query 'topic org-srs-review-source))

(defconst org-inc-extract-regexp (rx "@@inc:[[" (group (+? not-newline)) "][@@" (group (*? anychar)) "@@inc:]]@@"))

(defmacro org-inc-with-embed-cloze-vars (&rest body)
  (declare (indent 0))
  `(progn
     (defvar org-srs-embed-cloze-tag)
     (defvar org-srs-embed-cloze-brackets)
     (defvar org-srs-embed-cloze-overlay-category)
     (let ((org-srs-embed-cloze-tag "inc")
           (org-srs-embed-cloze-brackets '(?\[ ?\]))
           (org-srs-embed-cloze-overlay-category 'org-inc-extract))
       ,@body)))

(cl-defun org-inc-extract-put-overlays (&optional (start (point-min)) (end (point-max)))
  (org-inc-with-embed-cloze-vars (org-srs-embed-put-cloze-overlays start end)))

(cl-defun org-inc-extract-remove-overlays (&optional (start (point-min)) (end (point-max)))
  (org-inc-with-embed-cloze-vars (org-srs-embed-remove-cloze-overlays start end)))

(cl-defun org-inc-priorities (&optional (start (point-min)) (end (point-max)))
  (org-with-wide-buffer
   (save-excursion
     (cl-sort
      (cl-loop initially (goto-char start)
               while (re-search-forward org-priority-regexp end t)
               collect (cl-list* (string-to-number (match-string-no-properties 2)) (match-beginning 2) (match-end 2)))
      #'< :key #'car))))

(cl-defun org-inc-priority-put-overlays (&optional (start (point-min)) (end (point-max)))
  (cl-loop with data = (org-inc-priorities start end)
           with length = (length data)
           for (nil . (start . end)) in (org-inc-priorities)
           for overlay = (make-overlay start end)
           for index from 0
           do
           (overlay-put overlay 'category 'org-inc-priority)
           (overlay-put overlay 'invisible nil)
           (overlay-put overlay 'display (format "%.1f%%" (/ (* 10000 (float index)) (max 1 (1- length)) 100.0)))))

(cl-defun org-inc-priority-remove-overlays (&optional (start (point-min)) (end (point-max)))
  (remove-overlays start end 'category 'org-inc-priority))

(defun org-inc-priority-set (value)
  (interactive (list (float (read-number "Priority [0.0-1.0]: " 0.5))))
  (cl-etypecase value
    (float (let* ((priorities (org-inc-priorities))
                  (length (length priorities))
                  (nth (truncate (* (1- length) (min (max value 0.0) 1.0)))))
             (cl-destructuring-bind (start &optional (end (list org-inc-priority-max)) &rest args)
                 (if (zerop nth) (cons (list org-inc-priority-min) priorities) (nthcdr nth priorities))
               (setf (org-inc-priority-current) (org-inc-priority-between (car start) (car end))))))
    (integer (setf (org-inc-priority-current)
                   (cl-loop for previous = org-inc-priority-min then (if (= current value) previous current)
                            for (current . nil) in (org-inc-priorities)
                            when (< value current)
                            return (org-inc-priority-between previous current)
                            finally (cl-return (org-inc-priority-between current org-inc-priority-min))))))
  (when (called-interactively-p 'any) (org-inc-priority-update-overlays)))

(defvar org-inc-overlay-mode)

(cl-defun org-inc-priority-update-overlays (&rest args)
  (org-with-wide-buffer
   (apply #'org-inc-priority-remove-overlays args)
   (when org-inc-overlay-mode
     (apply #'org-inc-priority-put-overlays args))))

(defun org-inc-update-overlays ()
  (org-inc-extract-update-overlays)
  (org-inc-priority-update-overlays))

(cl-defmethod org-srs-item-new-interactively :after ((_type (eql 'topic)) &rest _args)
  (org-inc-priority-update-overlays))

(cl-defun org-inc-extract-update-overlays (&optional
                                           (start (save-excursion (org-end-of-meta-data t) (point)))
                                           (end (org-entry-end-position)))
  (org-inc-extract-remove-overlays start end)
  (when org-inc-overlay-mode
    (org-inc-extract-put-overlays start end)))

(cl-defun org-inc-remove-overlays (&optional (start (point-min)) (end (point-max)))
  (org-inc-extract-remove-overlays start end)
  (org-inc-priority-remove-overlays start end))

(cl-defun org-inc-put-overlays (&optional (start (point-min)) (end (point-max)))
  (org-inc-extract-put-overlays start end)
  (org-inc-priority-put-overlays start end))

;;;###autoload
(define-minor-mode org-inc-overlay-mode
  "Minor mode for visualizing the embedded Org-srs entries using overlays."
  :group 'org-inc
  (cl-assert (eq major-mode 'org-mode))
  (if org-inc-overlay-mode (org-inc-put-overlays) (org-inc-remove-overlays)))

(cl-defun org-inc-extract-bounds ()
  (org-inc-with-embed-cloze-vars
    (org-srs-embed-cloze-bounds (point) (org-entry-beginning-position) (org-entry-end-position))))

(defun org-inc-extract-delete ()
  (goto-char (car (org-inc-extract-bounds)))
  (cl-assert (looking-at org-inc-extract-regexp))
  (replace-match (match-string 2) t t))

(cl-defun org-inc-log-new-record (&rest args &key (timestamp (org-srs-item-due-timestamp)) action &allow-other-keys)
  (cl-assert action)
  (org-srs-table-goto-starred-line)
  (org-srs-table-with-temp-buffer
    (cl-loop for (name . value) in (save-excursion (forward-line -1) (org-srs-table-current-line))
             do (setf (org-srs-table-field name) (prin1-to-string value t)))
    (cl-loop initially  (setf (org-srs-item-due-timestamp) (org-srs-timestamp-now))
             for (name . value) in (org-srs-log-plist-alist args)
             do (setf name (intern (string-remove-prefix ":" (symbol-name name))))
             unless (eq name 'timestamp)
             do (setf (org-srs-table-field name) (prin1-to-string value t)))
    (org-srs-table-forward-star)
    (org-srs-table-goto-starred-line)
    (setf (org-srs-item-due-timestamp) timestamp)))

(cl-defun org-inc-extract (&optional (type (read (completing-read "Item type: " (org-srs-item-types) nil t nil nil '("topic")))))
  (org-inc-with-embed-cloze-vars
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
          (start (copy-marker (org-entry-beginning-position)))
          (end (save-excursion (org-end-of-subtree) (point-marker))))
      (cl-multiple-value-bind (id marker)
          (save-mark-and-excursion
            (let* ((level (org-current-level))
                   (a-factor (org-inc-topic-a-factor))
                   (a-factor-child (org-inc-a-factor-child a-factor))
                   (a-factor-parent (org-inc-a-factor-parent a-factor))
                   (timestamp (org-srs-item-with-current ('topic)
                                (prog2 (org-srs-table-goto-starred-line)
                                    (alist-get
                                     'timestamp
                                     (org-srs-algorithm-repeat
                                      'topic
                                      `((timestamp . ,(org-srs-timestamp-now))
                                        (timestamp . ,(org-srs-item-due-timestamp))
                                        (timestamp . ,(org-inc-last-review-timestamp))
                                        (a_factor . ,a-factor-child))))
                                  (org-inc-log-new-record :action :extract :a_factor a-factor-parent)))))
              (org-end-of-subtree t t)
              (org-insert-heading nil t (1+ level))
              (let ((marker (point-marker)))
                (org-return-and-maybe-indent)
                (insert text)
                (cl-values
                 (let ((org-inc-action :derive)
                       (org-inc-a-factor a-factor-child))
                   (prog1 (org-id-get-create)
                     (org-srs-item-new type)
                     (setf (org-srs-item-due-timestamp)
                           (cl-case type
                             (topic (org-srs-timestamp+ (org-srs-timestamp-now) (org-inc-a-factor-topic-initial-interval a-factor) :day))
                             (t timestamp)))
                     (org-inc-priority-set (1- (cl-loop for (priority . nil) in (org-inc-priorities start end) minimize priority)))
                     (org-srs-log-hide-drawer)))
                 marker))))
        (org-srs-embed-cloze (region-beginning) (region-end) nil (concat "id:" id))
        (org-inc-update-overlays)
        (cl-case type
          (topic)
          (t (goto-char marker) (end-of-line)))))))

;;;###autoload
(defun org-inc-extract-dwim ()
  (interactive)
  (require 'org-srs)
  (cond
   ((region-active-p) (org-inc-extract))
   (t (when-let ((position (car (org-inc-extract-bounds))))
        (goto-char position)
        (cl-assert (looking-at org-inc-extract-regexp))
        (cl-destructuring-bind (&optional (arg 1)) current-prefix-arg
          (cl-etypecase arg
            ((integer 4)
             (save-excursion
               (org-link-open-from-string (match-string 1))
               (org-cut-subtree))
             (org-inc-extract-delete)
             (org-inc-update-overlays))
            ((integer 1)
             (org-link-open-from-string (match-string 1)))))))))

(defconst org-inc-dismiss-timestamp "9999-99-99T99:99:99Z")

;;;###autoload
(cl-defun org-inc-dismiss ()
  (interactive)
  (org-srs-property-let ((org-srs-review-cache-p nil))
    (save-excursion
      (cl-loop for item in (org-srs-query '(and) (cons (org-entry-beginning-position) (org-entry-end-position)))
               do (org-srs-item-with-current item
                    (unless (org-srs-timestamp= (org-srs-item-due-timestamp) org-inc-dismiss-timestamp)
                      (org-inc-log-new-record :timestamp org-inc-dismiss-timestamp :action :dismiss))))
      (org-srs-log-hide-drawer)))
  (setf (org-inc-priority-current) nil)
  (when (called-interactively-p 'any) (org-inc-update-overlays))
  (when (org-srs-reviewing-p)
    (let ((org-inc-continue nil))
      (org-inc-continue))
    (org-srs-review-next)))

;;;###autoload
(cl-defun org-inc-postpone (&optional (time '(1 :day)))
  (interactive (list (read-from-minibuffer "Interval: " (prin1-to-string '(1 :day)) nil t)))
  (org-inc-log-new-record :action :postpone)
  (org-srs-review-postpone time))

(defun org-inc-continue (&rest args)
  (interactive)
  (let ((item (or org-srs-review-item args))
        (org-inc-continue (if (boundp 'org-inc-continue) org-inc-continue t)))
    (org-srs-item-with-current item
      (cl-case (car (ensure-list (cl-first args)))
        (topic (funcall
                (if (called-interactively-p 'any) #'call-interactively #'funcall)
                #'org-srs-item-confirm-command))
        (t (if (and (called-interactively-p 'any) (commandp org-srs-item-confirm))
               (call-interactively org-srs-item-confirm)
             (apply org-srs-item-confirm args)))))))

;; TODO 多优先级
;; DONE 新加入的优先级应当在最后一个子 item 之前并且当前 item 之前
;; DONE 当 topic 被 review 时，在优先队列中与下一个非 dismissed 元素交换

(add-hook 'org-mode-hook #'org-inc-overlay-mode)

(cl-defun org-inc-change-priority-after-rate ()
  (when-let ((priority (org-inc-priority-current)))
    (cl-multiple-value-bind (start end)
        (cl-loop for ((current . nil) (next . nil) (next-next . nil)) on (org-inc-priorities)
                 when (= current priority)
                 if next-next return (cl-values next next-next)
                 else if next return (cl-values next org-inc-priority-max)
                 else do (cl-return-from org-inc-change-priority-after-rate)
                 finally (cl-assert nil))
      (setf (org-inc-priority-current) (org-inc-priority-between start end))
      (org-inc-priority-update-overlays))))

(add-hook 'org-srs-review-after-rate-hook #'org-inc-change-priority-after-rate)

(provide 'org-inc)
;;; org-inc.el ends here
