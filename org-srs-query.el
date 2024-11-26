;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-item)

(defun org-srs-query-predicate-and (&rest predicates)
  (lambda () (cl-loop for predicate in predicates always (funcall predicate))))

(defun org-srs-query-predicate-or (&rest predicates)
  (lambda () (cl-loop for predicate in predicates thereis (funcall predicate))))

(defun org-srs-query-predicate-not (predicate)
  (lambda () (lambda () (not (funcall predicate)))))

(defun org-srs-query-predicate-new ()
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (forward-line -1)
        (or (org-at-table-hline-p) (string-empty-p (org-srs-table-field 'rating)))))))

(cl-defun org-srs-query-predicate-updated
    (&optional
     (from (org-srs-time-truncate-hms (current-time)) fromp)
     (to (unless fromp (org-srs-time-truncate-hms (current-time) 1))))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (forward-line -1)
        (unless (org-at-table-hline-p)
          (let ((time (parse-iso8601-time-string (org-srs-table-field 'timestamp))))
            (and (time-less-p from time) (or (null to) (time-less-p time to)))))))))

(cl-defun org-srs-query-predicate-due (&optional (now (current-time)))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (time-less-p (parse-iso8601-time-string (match-string 2)) now)))))

(cl-defun org-srs-query-predicate-learned
    (&optional
     (from (org-srs-time-truncate-hms (current-time)) fromp)
     (to (unless fromp (org-srs-time-truncate-hms (current-time) 1))))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (when-let ((time (cl-loop with end = (org-table-end)
                                  initially (org-table-goto-line 2)
                                  when (org-srs-table-field 'rating)
                                  return (org-srs-table-field 'timestamp)
                                  do (forward-line 1)
                                  until (> (point) end))))
          (and (time-less-p from time) (or (null to) (time-less-p time to))))))))

(defun org-srs-query-predicate-reviewed (&rest args)
  (org-srs-query-predicate-and
   (org-srs-query-predicate-not (org-srs-query-predicate-new))
   (apply #'org-srs-query-predicate-updated args)))

(cl-defun org-srs-query-buffer (predicate &optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (cl-loop initially (goto-char (point-min))
               while (re-search-forward org-srs-item-header-regexp nil t)
               when (save-match-data (funcall predicate))
               collect (cl-values-list (org-srs-item-from-match-data))))))

(provide 'org-srs-query)
