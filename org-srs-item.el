;;; org-srs-item-cloze.el --- Interface for review items -*- lexical-binding:t -*-

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

;; This package serves as the review item interface for Org-srs and
;; provides common functions, allowing the core to be independent of
;; specific item types.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)
(require 'custom)

(require 'org)
(require 'org-element)

(require 'org-srs-property)
(require 'org-srs-log)

(defgroup org-srs-item nil
  "Interface for various types of review items."
  :group 'org-srs
  :prefix "org-srs-item-")

(defalias 'org-srs-item-begin 'org-srs-log-begin)
(defalias 'org-srs-item-end 'org-srs-log-end)

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

(defun org-srs-item-link-search (s)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (rx bol "#+NAME:" (+ blank) (literal s) eol)))
  (goto-char (match-beginning 0)))

(cl-defun org-srs-item-goto (item &optional (id (org-id-get)) (buffer (current-buffer)))
  (let ((org-link-search-must-match-exact-headline t))
    (unless (eq buffer (current-buffer))
      (cl-assert (eq (window-buffer) (current-buffer)))
      (switch-to-buffer buffer nil t)
      (cl-assert (eq (window-buffer) buffer)))
    (cl-assert (eq (current-buffer) buffer))
    (org-srs-item-link-search (org-srs-item-link item id))))

(defun org-srs-item-exists-p (item &rest args)
  (save-excursion
    (ignore-error error
      (apply #'org-srs-item-goto item args)
      t)))

(cl-defun org-srs-item-due-timestamp (&optional (item nil itemp) &rest args)
  (save-window-excursion
    (when itemp (apply #'org-srs-item-goto item args))
    (re-search-forward org-srs-log-latest-timestamp-regexp (org-srs-table-end))
    (match-string 2)))

(defun org-srs-item-repeat (item rating)
  (org-srs-item-goto item)
  (org-srs-log-repeat rating))

(defconst org-srs-item-header-regexp (rx bol (* blank) "#+NAME: " (* blank) (regexp org-srs-item-regexp) (* blank) eol))

(defun org-srs-item-from-match-data ()
  (let ((id (match-string-no-properties 1)))
    (cl-values (when (match-string 2) (mapcar #'read (split-string (match-string 2) "::"))) id)))

(defun org-srs-item-at-point ()
  (save-excursion
    (goto-char (org-srs-item-begin))
    (when (re-search-forward org-srs-item-header-regexp (org-srs-item-end) t)
      (org-srs-item-from-match-data))))

(cl-defun org-srs-item-bounds (&optional (item (cl-nth-value 0 (org-srs-item-at-point))) &rest args)
  (save-excursion
    (apply #'org-srs-item-goto item args)
    (let ((element (org-element-at-point)))
      (cons (org-element-begin element) (org-element-end element)))))

(cl-defun org-srs-item-delete (&rest args)
  (cl-destructuring-bind (start . end) (apply #'org-srs-item-bounds args)
    (delete-region start end)))

(cl-defgeneric org-srs-item-review (type &rest args))

(cl-defgeneric org-srs-item-new (type &rest args)
  (let ((item (cons type args)))
    (cl-assert (not (org-srs-item-exists-p item)) nil "Item %s already exists" item)
    (cl-assert (org-id-get))
    (org-srs-log-end-of-drawer)
    (org-open-line 1)
    (apply #'org-srs-item-insert type args)))

(defun org-srs-item-types ()
  (cl-delete-duplicates
   (cl-loop for gf in '(org-srs-item-review org-srs-item-new)
            nconc (cl-loop for method in (cl--generic-method-table (cl-generic-ensure-function gf))
                           for (eql symbol) = (ensure-list (cl-first (cl--generic-method-specializers method)))
                           when (eq eql 'eql)
                           do (cl-assert (eq (cl-first symbol) 'quote))
                           and collect (cl-second symbol)))))

(cl-defgeneric org-srs-item-new-interactively (type &rest args)
  (apply #'org-srs-item-new type args))

(cl-defmethod org-srs-item-new-interactively :around (_type &rest _args)
  (save-excursion
    (cl-call-next-method)
    (org-srs-log-hide-drawer)))

;;;###autoload
(defun org-srs-item-create ()
  "Create a review item in the current entry."
  (interactive)
  (require 'org-srs)
  (org-srs-item-new-interactively
   (prog1 (read (completing-read "Item type: " (org-srs-item-types) nil t))
     (org-id-get-create))))

(cl-defun org-srs-item-add-hook-once (hook function &optional depth (local t))
  (add-hook
   hook
   (letrec ((hook-function (lambda () (remove-hook hook hook-function local) (funcall function))))
     hook-function)
   depth local))

(defun org-srs-item-run-hook-once (hook)
  (if (local-variable-p hook)
      (let ((cons-set (cl-loop with table = (make-hash-table :test #'eq)
                               for cons on (symbol-value hook)
                               do (setf (gethash cons table) t)
                               finally (cl-return table))))
        (unwind-protect (run-hooks hook)
          (when (cl-loop for cons on (symbol-value hook) always (gethash cons cons-set))
            (kill-local-variable hook))))
    (run-hooks hook)))

(defun org-srs-item-run-hooks-once (&rest hooks)
  (mapc #'org-srs-item-run-hook-once hooks))

(defun org-srs-item-narrow ()
  (org-back-to-heading)
  (org-narrow-to-subtree)
  (org-srs-item-add-hook-once 'org-srs-review-after-rate-hook #'widen))

(defvar org-srs-item-before-confirm-hook nil)
(defvar org-srs-item-after-confirm-hook nil)

(defun org-srs-item-confirm-read-key (&rest _args)
  (org-srs-item-run-hooks-once 'org-srs-item-before-confirm-hook)
  (read-key "Continue with any key")
  (org-srs-item-run-hooks-once 'org-srs-item-after-confirm-hook))

(defun org-srs-item-confirm-command (&rest _args)
  "Continue the item being reviewed in the current review session.

This command is intended to be used only when customizable option
`org-srs-item-confirm' is set to `org-srs-item-confirm-command' for
the current item."
  (interactive)
  (let ((flag-hook (eval-when-compile (letrec ((hook (lambda () (remove-hook 'org-srs-item-after-confirm-hook hook t)))) hook))))
    (if (member flag-hook org-srs-item-after-confirm-hook)
        (org-srs-item-run-hook-once 'org-srs-item-after-confirm-hook)
      (cl-assert (not (called-interactively-p 'any)))
      (org-srs-item-run-hooks-once 'org-srs-item-before-confirm-hook)
      (message (substitute-command-keys "Continue with \\[org-srs-item-confirm-command]"))
      (add-hook 'org-srs-item-after-confirm-hook flag-hook nil t))))

(defun org-srs-item-confirm-cleanup-on-quit ()
  (cl-loop for hook in '(org-srs-item-before-confirm-hook org-srs-item-after-confirm-hook)
           when (local-variable-p hook)
           when (boundp 'org-srs-review-rating)
           do (cl-assert (null (symbol-value 'org-srs-review-rating)))
           end and
           do (org-srs-item-run-hooks-once hook)))

(add-hook 'org-srs-review-before-rate-hook #'org-srs-item-confirm-cleanup-on-quit)

(org-srs-property-defcustom org-srs-item-confirm #'org-srs-item-confirm-read-key
  "Method to confirm the current item and reveal its answer."
  :group 'org-srs-item
  :type 'function)

(provide 'org-srs-item)
;;; org-srs-item.el ends here
