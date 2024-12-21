;;; org-srs-property.el --- Property-based customization facilities -*- lexical-binding:t -*-

;; Copyright (C) 2024 Bohong Huang

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

;; This package provides support of defining, accessing, and modifying
;; global, per-buffer, per-heading, per-item, and even per-control
;; flow customizable variables for Org-srs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'org)
(require 'org-element)

(defun org-srs-property-plist-at-point ()
  (save-excursion
    (when (org-at-table-p)
      (goto-char (org-table-begin)))
    (let ((element (org-element-at-point)))
      (when (eq (org-element-type element) 'table)
        (when-let ((property (org-element-property :attr_srs element)))
          (read (concat "(" (cl-reduce (lambda (acc it) (concat acc " " it)) property) ")")))))))

(defmacro org-srs-property-defcustom (name &rest defcustom-args)
  (declare (doc-string 3) (indent defun))
  (cl-assert (string-prefix-p (symbol-name 'org-srs) (symbol-name name)))
  (let* ((property (string-remove-prefix (symbol-name 'org-) (symbol-name name)))
         (property-name (string-replace "-" "_" (upcase property)))
         (property (string-remove-prefix (symbol-name 'srs-) property))
         (propname (intern (concat ":" property))))
    (cl-with-gensyms (value anonymous-variable thunk)
      `(progn
         (defcustom ,name . ,defcustom-args)
         (cl-defun ,name (&optional ,value ,thunk)
           (if ,thunk
               (cl-locally
                (defvar ,anonymous-variable)
                (let ((,anonymous-variable ,value))
                  (funcall ,thunk)))
             (when (boundp ',anonymous-variable)
               (cl-return-from ,name (symbol-value ',anonymous-variable)))
             (when (eq major-mode 'org-mode)
               (let* ((,value (cl-getf (org-srs-property-plist-at-point) ,propname ',anonymous-variable)))
                 (unless (eq ,value ',anonymous-variable)
                   (cl-return-from ,name ,value)))
               (when-let ((,value (org-entry-get nil ,property-name t t)))
                 (cl-return-from ,name (read ,value))))
             ,name))))))

(defmacro org-srs-property-let (bindings &rest body)
  (declare (indent 1))
  (if bindings
      (cl-destructuring-bind ((var val) . rest) bindings
        `(,var ,val (lambda () (org-srs-property-let ,rest . ,body))))
    `(progn . ,body)))

(provide 'org-srs-property)
;;; org-srs-property.el ends here
