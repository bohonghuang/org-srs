;;; org-srs-item-cloze.el --- Interface for review items -*- lexical-binding:t -*-

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

;; This package serves as the review item interface for Org-srs and
;; provides common functions, allowing the core to be independent of
;; specific item types.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)

(require 'org)
(require 'org-element)

(require 'org-srs-property)
(require 'org-srs-log)

(defconst org-srs-item-regexp (rx "srsitem:" (group (+ (not (any ?: blank control)))) (? "::" (group (+ (not (any blank control)))))))

(defun org-srs-item-name ()
  (let ((id (org-id-get)))
    (cl-assert id)
    id))

(defun org-srs-item-princ-to-string (object)
  (if (and (symbolp object) (not (eq (read (symbol-name object)) object)))
      (format "\\%s" object)
    (format "%s" object)))

(cl-defun org-srs-item-link (item &optional (id (org-id-get)))
  (cl-reduce
   (lambda (acc it) (format "%s::%s" acc (org-srs-item-princ-to-string it)))
   (cl-delete nil (ensure-list item) :end 1)
   :initial-value (format "srsitem:%s" id)))

(defun org-srs-item-insert (item &rest args)
  (insert "#+NAME: " (apply #'org-srs-item-link item args))
  (newline-and-indent)
  (org-srs-log-insert))

(defun org-srs-item-goto (item &rest args)
  (let ((org-link-search-must-match-exact-headline t))
    (org-link-search (apply #'org-srs-item-link item args))
    (end-of-line)))

(defun org-srs-item-exists-p (item &rest args)
  (save-excursion
    (ignore-error error
      (apply #'org-srs-item-goto item args)
      t)))

(defun org-srs-item-repeat (item rating)
  (org-srs-item-goto item)
  (org-srs-log-repeat rating))

(defconst org-srs-item-header-regexp (rx bol (* blank) "#+NAME: " (* blank) (regexp org-srs-item-regexp) (* blank) eol))

(defun org-srs-item-from-match-data ()
  (let ((id (match-string-no-properties 1)))
    (cl-values (when (match-string 2) (mapcar #'read (split-string (match-string 2) "::"))) id)))

(defun org-srs-item-at-point ()
  (save-excursion
    (when (or (org-at-table-p) (looking-at-p org-srs-item-header-regexp) (looking-back org-srs-item-header-regexp (pos-bol)))
      (goto-char (org-table-begin))
      (let ((element (org-element-at-point)))
        (goto-char (org-element-begin element))
        (when (re-search-forward org-srs-item-header-regexp (org-element-end element) t)
          (org-srs-item-from-match-data))))))

(cl-defun org-srs-item-bounds (&optional (item (cl-nth-value 0 (org-srs-item-at-point))) &rest args)
  (save-excursion
    (apply #'org-srs-item-goto item args)
    (let ((element (org-element-at-point)))
      (cons (org-element-begin element) (org-element-end element)))))

(cl-defun org-srs-item-delete (&rest args)
  (cl-destructuring-bind (start . end) (apply #'org-srs-item-bounds args)
    (delete-region start end)))

(cl-defgeneric org-srs-item-review (type &rest args))

(defun org-srs-item-types ()
  (cl-loop for method in (cl--generic-method-table (cl-generic-ensure-function 'org-srs-item-review))
           for (eql symbol) = (ensure-list (cl-first (cl--generic-method-specializers method)))
           when (eq eql 'eql)
           do (cl-assert (eq (cl-first symbol) 'quote))
           and collect (cl-second symbol)))

(cl-defgeneric org-srs-item-new (type &rest args)
  (let ((item (cons type args)))
    (cl-assert (not (org-srs-item-exists-p item)) nil "Item %s already exists" item)
    (org-srs-log-end-of-drawer)
    (org-open-line 1)
    (apply #'org-srs-item-insert type args)))

(cl-defgeneric org-srs-item-new-interactively (type &rest args)
  (apply #'org-srs-item-new type args))

(cl-defmethod org-srs-item-new-interactively :around (_type &rest _args)
  (save-excursion
    (cl-call-next-method)
    (org-srs-log-hide-drawer)))

(defun org-srs-item-create ()
  "Create a review item."
  (interactive)
  (org-srs-item-new-interactively (read (completing-read "Item type: " (org-srs-item-types) nil t))))

(defun org-srs-item-add-hook-once (hook function &rest args)
  (apply
   #'add-hook hook
   (letrec ((hook-function (lambda ()
                             (remove-hook hook hook-function)
                             (funcall function))))
     hook-function)
   args))

(defun org-srs-item-narrow ()
  (org-back-to-heading)
  (org-narrow-to-subtree)
  (org-srs-item-add-hook-once 'org-srs-review-after-rate-hook #'widen))

(defvar org-srs-item-after-confirm-hook nil)

(defun org-srs-item-reset-after-confirm-hook ()
  (setf org-srs-item-after-confirm-hook nil))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-item-reset-after-confirm-hook)

(defun org-srs-item-confirmation-read-key (&rest _args)
  (read-key "Press any key to continue")
  (run-hooks 'org-srs-item-after-confirm-hook))

(org-srs-property-defcustom org-srs-item-confirmation #'org-srs-item-confirmation-read-key
  "The method to confirm the current item and reveal its answer."
  :group 'org-srs
  :type 'function)

(provide 'org-srs-item)
;;; org-srs-item.el ends here
