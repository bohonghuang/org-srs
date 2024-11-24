;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-srs-log)

(cl-defun org-srs-review-buffer-due-timestamp-markers ()
  (save-excursion
    (cl-loop initially (goto-char (point-min))
             for end = (or (re-search-forward org-srs-log-latest-timestamp-regexp nil t) (cl-return markers))
             for start = (match-beginning 2)
             for time = (parse-iso8601-time-string (match-string 2))
             when (time-less-p time nil)
             collect (cons time (copy-marker start)) into markers)))

(cl-defun org-srs-review (&optional (source (buffer-file-name)))
  (cl-etypecase source
    (string
     (cl-assert (file-exists-p source))
     (cl-assert (not (file-directory-p source)))
     (with-current-buffer (find-file-noselect source)
       (org-srs-review-buffer-due-timestamp-markers)))))

(provide 'org-srs-review)
