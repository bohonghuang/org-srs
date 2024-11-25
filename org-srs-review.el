;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-srs-log)


(cl-defun org-srs-review (&optional (source (buffer-file-name)))
  (cl-etypecase source
    (string
     (cl-assert (file-exists-p source))
     (cl-assert (not (file-directory-p source)))
     (with-current-buffer (find-file-noselect source)
       (org-srs-review-buffer-due-timestamp-markers)))))

(provide 'org-srs-review)
