;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-srs-log)
(require 'org-srs-item)

(cl-defun org-srs-query-predicate-due-timestamp (&optional (less-than (current-time)))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (time-less-p (parse-iso8601-time-string (match-string 2)) less-than)))))

(cl-defun org-srs-query-buffer (predicate &optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (cl-loop initially (goto-char (point-min))
             while (re-search-forward org-srs-item-header-regexp nil t)
             when (save-match-data (funcall predicate))
             collect (cl-values-list (org-srs-item-from-match-data)))))

(provide 'org-srs-query)
