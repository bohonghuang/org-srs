;;; org-srs-property.el --- Property-based customization facilities -*- lexical-binding:t -*-

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

;; This package provides support of defining, accessing, and modifying
;; global, per-buffer, per-heading, per-item, and even per-control
;; flow customizable variables for Org-srs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'custom)
(require 'cus-edit)

(require 'org)
(require 'org-element)

(require 'org-srs-table)

(defgroup org-srs nil
  "Spaced repetition system inside Org-mode."
  :group 'org
  :prefix "org-srs-"
  :link '(url-link "https://github.com/bohonghuang/org-srs"))

(defun org-srs-property-plist-at-point ()
  (ignore-error error
    (save-excursion
      (goto-char (org-srs-table-element-begin))
      (when-let ((property (org-element-property :attr_srs (org-element-at-point))))
        (read (concat "(" (cl-reduce (lambda (acc it) (concat acc " " it)) property) ")"))))))

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
         (put ',name 'safe-local-variable #'always)
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

(cl-defun org-srs-property-group-members (&optional (group 'org-srs))
  (cl-loop for (member type) in (custom-group-members group nil)
           if (eq type 'custom-variable)
           collect member
           else if (eq type 'custom-group)
           nconc (org-srs-property-group-members member)
           else
           do (cl-assert nil)))

(cl-defun org-srs-property-call-with-saved-properties (thunk &optional (properties (org-srs-property-group-members)))
  (cl-destructuring-bind (property . properties) properties
    (funcall
     property (funcall property)
     (if properties (apply-partially #'org-srs-property-call-with-saved-properties thunk properties) thunk))))

(cl-define-compiler-macro org-srs-property-call-with-saved-properties (&whole form thunk &optional properties)
  (pcase properties
    (`'(,property) `(org-srs-property-let ((,property (,property))) (funcall ,thunk)))
    (`'() `(funcall ,thunk))
    (_ form)))

(defmacro org-srs-property-let (bindings &rest body)
  (declare (indent 1))
  (pcase-exhaustive bindings
    (`((,(and (pred symbolp) var) ,val) . ,rest)
     `(,var ,val (lambda () (org-srs-property-let ,rest . ,body))))
    (`() `(progn . ,body))
    (`(,(and (pred symbolp) var) . ,rest)
     `(org-srs-property-call-with-saved-properties (lambda () (org-srs-property-let ,rest . ,body)) '(,var)))
    ((and (pred symbolp) (or (and 't (let group 'org-srs)) group))
     `(org-srs-property-call-with-saved-properties (lambda () . ,body) (org-srs-property-group-members ',group)))))

(provide 'org-srs-property)
;;; org-srs-property.el ends here
