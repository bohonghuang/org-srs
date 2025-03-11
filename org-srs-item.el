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

(cl-defun org-srs-item-goto (item &optional (id (org-id-get)) (buffer (current-buffer)))
  (let ((org-link-search-must-match-exact-headline t))
    (unless (eq buffer (current-buffer))
      (cl-assert (eq (window-buffer) (current-buffer)))
      (switch-to-buffer buffer nil t)
      (cl-assert (eq (window-buffer) buffer)))
    (cl-assert (eq (current-buffer) buffer))
    (org-link-search (org-srs-item-link item id))
    (end-of-line)))

(defun org-srs-item-exists-p (item &rest args)
  (save-excursion
    (ignore-error error
      (apply #'org-srs-item-goto item args)
      t)))

(cl-defun org-srs-item-due-timestamp (&optional (item nil itemp) &rest args)
  (save-window-excursion
    (when itemp (apply #'org-srs-item-goto item args))
    (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
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
  (cl-assert (buffer-file-name (buffer-base-buffer))
             nil "Buffer should be visiting a file")
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
  (cl-assert (local-variable-p hook))
  (let ((cons-set (cl-loop with table = (make-hash-table :test #'eq)
                           for cons on (symbol-value hook)
                           do (setf (gethash cons table) t)
                           finally (cl-return table))))
    (unwind-protect (run-hooks hook)
      (when (cl-loop for cons on (symbol-value hook) always (gethash cons cons-set))
        (kill-local-variable hook)))))

(defun org-srs-item-run-hooks-once (&rest hooks)
  (mapc #'org-srs-item-run-hook-once hooks))

(defun org-srs-item-narrow ()
  (org-back-to-heading)
  (org-narrow-to-subtree)
  (org-srs-item-add-hook-once 'org-srs-review-after-rate-hook #'widen))

(defvar org-srs-item-after-confirm-hook nil)

(defun org-srs-item-confirmation-read-key (&rest _args)
  (read-key "Press any key to continue")
  (org-srs-item-run-hooks-once 'org-srs-item-after-confirm-hook))

(defvar-local org-srs-item-wait-position nil
  "Used by `org-srs-item-confirmation-wait' to store point positions. That function should be the only thing that ever locally binds this variable.")

(defun org-srs-item-confirmation-wait (&rest _args)
  "Associate the current value of point with the current buffer, then tell the user to call `org-srs-item-confirmation-go' to continue.

This function is meant to be a possible value of `org-srs-item-confirmation'."
  (setq org-srs-item-wait-position (point))
  (message "Call `org-srs-item-confirmation-go' in this buffer to reveal this item."))

(defun org-srs-item-confirmation-go ()
  "If the current buffer has a running review session, restore point and reveal the answer. Otherwise, notify the user and do nothing.

See also `org-srs-item-confirmation-wait'."
  (interactive)
  (cl-assert (org-srs-reviewing-p))
  (if org-srs-item-wait-position
      (progn
        (goto-char org-srs-item-wait-position)
        (setq org-srs-item-wait-position nil)
        (org-srs-item-run-hooks-once 'org-srs-item-after-confirm-hook))
    (message "This buffer contains no items that are presented for review.")))

(org-srs-property-defcustom org-srs-item-confirmation #'org-srs-item-confirmation-read-key
  "The method to confirm the current item and reveal its answer."
  :group 'org-srs
  :type 'function)

(provide 'org-srs-item)
;;; org-srs-item.el ends here
