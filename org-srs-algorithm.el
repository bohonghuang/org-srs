;; -*- lexical-binding: t; -*-

(require 'cl-generic)

(require 'org-srs-property)

(org-srs-property-defcustom org-srs-algorithm nil "")

(cl-defgeneric org-srs-algorithm-repeat (algorithm args))

(provide 'org-srs-algorithm)
