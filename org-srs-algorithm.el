;; -*- lexical-binding: t; -*-

(require 'cl-generic)

(defcustom org-srs-algorithm nil "")

(defun org-srs-algorithm ()
  (or org-srs-algorithm))

(cl-defgeneric org-srs-algorithm-repeat (algorithm args))

(provide 'org-srs-algorithm)
