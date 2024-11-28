;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-srs-log)
(require 'org-srs-query)
(require 'org-srs-item)

(defcustom org-srs-review-new-items-per-day 20 "")
(defcustom org-srs-review-max-reviews-per-day 200 "")
(defcustom org-srs-review-new-items-ignore-review-limit-p nil "")

(cl-defun org-srs-review-pending-items (&optional (source (current-buffer)))
  (cl-etypecase source
    (buffer
     (with-current-buffer source
       (let ((items-learned (org-srs-query-buffer (org-srs-query-predicate-learned)))
             (items-to-review (org-srs-query-predicate-and
                               (org-srs-query-predicate-due)
                               (org-srs-query-predicate-not (org-srs-query-predicate-reviewed))
                               (org-srs-query-predicate-not (org-srs-query-predicate-new))))
             (items-reviewed (org-srs-query-buffer (org-srs-query-predicate-reviewed)))
             (predicate-null (org-srs-query-predicate-or))
             (predicate-due-new (org-srs-query-predicate-and
                                 (org-srs-query-predicate-due)
                                 (org-srs-query-predicate-new)))
             (predicate-due-nonnew (org-srs-query-predicate-and
                                    (org-srs-query-predicate-due)
                                    (org-srs-query-predicate-not
                                     (org-srs-query-predicate-new))))
             (predicate-due-today (org-srs-query-predicate-due))
             (predicate-reviewed-due-today (org-srs-query-predicate-and
                                            (org-srs-query-predicate-reviewed)
                                            (org-srs-query-predicate-due (org-srs-time-tomorrow)))))
         (or
          (org-srs-query-buffer
           (if (< (length items-reviewed) org-srs-review-max-reviews-per-day)
               (if (< (length items-learned) org-srs-review-new-items-per-day)
                   (if (or org-srs-review-new-items-ignore-review-limit-p
                           (< (+ (length items-reviewed) (length items-to-review))
                              org-srs-review-max-reviews-per-day))
                       predicate-due-today
                     predicate-due-nonnew)
                 predicate-due-nonnew)
             (if (< (length items-learned) org-srs-review-new-items-per-day)
                 (if org-srs-review-new-items-ignore-review-limit-p
                     predicate-due-new
                   predicate-null)
               predicate-null)))
          (org-srs-query-buffer predicate-reviewed-due-today)))))
    (string
     (cl-assert (file-exists-p source))
     (cl-assert (not (file-directory-p source)))
     (org-srs-review-pending-items (find-file-noselect source)))))

(defvar org-srs-review-item-marker nil)

(defun org-srs-review (&rest args)
  (interactive)
  (if-let ((item-and-id (cl-first (apply #'org-srs-review-pending-items args))))
      (cl-destructuring-bind (item id) item-and-id
        (apply #'org-srs-item-goto item-and-id)
        (setf org-srs-review-item-marker (point-marker))
        (org-srs-log-hide-drawer org-srs-review-item-marker)
        (apply #'org-srs-item-review (car item) (cdr item))
        (org-srs-log-hide-drawer org-srs-review-item-marker)
        (add-hook
         'org-srs-review-after-rate-hook
         (letrec ((review (lambda ()
                            (remove-hook 'org-srs-review-after-rate-hook review)
                            (apply #'org-srs-review args))))
           review)))
    (message "Review done")))

(defconst org-srs-review-ratings '(:easy :good :hard :again))

(defvar org-srs-review-after-rate-hook nil)

(defun org-srs-review-rate (rating &optional position)
  (interactive (list (read (completing-read "Rating: " org-srs-review-ratings nil t))))
  (save-excursion
    (if-let ((position (or position org-srs-review-item-marker)))
        (goto-char position)
      (cl-multiple-value-call #'org-srs-item-goto (org-srs-item-at-point)))
    (org-srs-table-goto-starred-line)
    (cl-assert
     (time-less-p
      (parse-iso8601-time-string (org-srs-table-field 'timestamp))
      (org-srs-time-tomorrow)))
    (org-srs-item-repeat (cl-nth-value 0 (org-srs-item-at-point)) rating)
    (org-srs-log-hide-drawer)
    (setf org-srs-review-item-marker nil))
  (run-hooks 'org-srs-review-after-rate-hook))

(defmacro org-srs-review-define-rating-commands ()
  `(progn . ,(cl-loop for rating in org-srs-review-ratings
                      collect `(defun ,(intern (format "%s%s" 'org-srs-review-rate- (string-trim (symbol-name rating) ":"))) ()
                                 (interactive)
                                 (org-srs-review-rate ,rating)))))

(org-srs-review-define-rating-commands)

(provide 'org-srs-review)
