;; -*- lexical-binding: t; -*-

(require 'org-srs-log)

(defun org-srs-item-name ()
  (let ((id (org-id-get)))
    (cl-assert id)
    id))

(defun org-srs-item-link (name)
  (format "srsitem:%s" name))

(defun org-srs-item-insert (name)
  (insert "#+NAME: " (org-srs-item-link name))
  (newline-and-indent)
  (org-srs-log-insert))

(defun org-srs-item-repeat (name rating)
  (cl-assert (org-link-search (org-srs-item-link name)))
  (forward-line 1)
  (org-srs-log-repeat rating))

(provide 'org-srs-item)
