;;; org-srs-table.el --- Table operation utilities -*- lexical-binding:t -*-

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

;; This package provides various functions to manipulate the tables of
;; review logs in Org-srs.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'nadvice)

(require 'org)
(require 'org-table)
(require 'org-element)

(defmacro org-srs-log-define-org-element-bound-functions ()
  `(progn
     ,@(unless (fboundp 'org-element-begin)
         `((defsubst org-element-begin (node)
             (org-element-property :begin node))
           (defsubst \(setf\ org-element-begin\) (value node)
             (setf (org-element-property :begin node) value))))
     ,@(unless (fboundp 'org-element-end)
         `((defsubst org-element-end (node)
             (org-element-property :end node))
           (defsubst \(setf\ org-element-end\) (value node)
             (setf (org-element-property :end node) value))))))

(org-srs-log-define-org-element-bound-functions)

(defun org-srs-table-column-name-number-alist ()
  (org-table-analyze)
  (cl-loop for (name . column-number) in org-table-column-names
           for number from 2
           do (cl-assert (string-equal (prin1-to-string number) column-number))
           collect (cons (intern name) number)))

(defconst org-srs-table-readable-field-regexp (rx bos (or (and ":" (+ anychar)) (and (? "-") (+ digit) (? "." (* digit)))) eos))

(defun org-srs-table-ensure-read-field (field)
  (if (string-match-p org-srs-table-readable-field-regexp field)
      (car (read-from-string field)) field))

(cl-defun org-srs-table-lines ()
  (cl-loop with names = (mapcar #'car (org-srs-table-column-name-number-alist))
           for line in (cdr (org-table-to-lisp))
           when (listp line)
           collect (cl-loop for name in names
                            for field in (cdr line)
                            unless (string-empty-p field)
                            collect (cons name (org-srs-table-ensure-read-field field)))))

(cl-defun org-srs-table-element-begin (&optional (element (org-element-at-point-no-context)))
  (cl-ecase (car element)
    (table (org-element-begin element))
    (table-row (org-srs-table-element-begin (org-element-parent element)))))

(cl-defun org-srs-table-element-end (&optional (element (org-element-at-point-no-context)))
  (cl-ecase (car element)
    (table (org-element-end element))
    (table-row (org-srs-table-element-end (org-element-parent element)))))

(defun org-srs-table-begin ()
  (if (looking-at-p org-table-line-regexp)
      (org-table-begin)
    (save-excursion
      (unless (and (looking-at-p org-keyword-regexp) (not (looking-at-p org-TBLFM-regexp)))
        (goto-char (org-srs-table-element-begin)))
      (re-search-forward org-table-line-regexp)
      (pos-bol))))

(defun org-srs-table-end ()
  (if (looking-at-p org-table-line-regexp)
      (org-table-end)
    (save-excursion
      (goto-char (org-srs-table-begin))
      (org-table-end))))

(defconst org-srs-table-starred-line-regexp (rx bol (* blank) "|" (* blank) (group "*") (* blank) "|"))

(defun org-srs-table-goto-starred-line ()
  (prog2 (goto-char (org-srs-table-begin))
      (re-search-forward org-srs-table-starred-line-regexp (org-srs-table-end))
    (goto-char (match-beginning 1))))

(cl-defun org-srs-table-current-line (&optional (columns (org-srs-table-column-name-number-alist)))
  (let ((line (org-table-current-line)))
    (cl-loop for (name . number) in columns
             for field in (org-table-get-range (format "@%d$%d..@%d$%d" line (1+ 1) line (1+ (length columns))))
             unless (string-empty-p field)
             collect (cons name (org-srs-table-ensure-read-field field)))))

(cl-defun org-srs-table-starred-line (&optional (offset 0))
  (save-excursion
    (when (org-srs-table-goto-starred-line)
      (cond
       ((cl-plusp offset)
        (cl-loop repeat offset do (org-table-next-row)))
       ((cl-minusp offset)
        (forward-line offset)))
      (org-srs-table-current-line))))

(defun org-srs-table-forward-star ()
  (when (org-srs-table-goto-starred-line)
    (goto-char (match-beginning 1))
    (save-mark-and-excursion
      (deactivate-mark)
      (org-table-rotate-recalc-marks " "))
    (org-table-next-row)
    (save-mark-and-excursion
      (deactivate-mark)
      (org-table-rotate-recalc-marks "*"))))

(defun org-srs-table-from-alist (alist)
  (cl-loop initially (insert "| ! |")
           for (name . nil) in alist
           do (insert (prin1-to-string name t) " | "))
  (newline)
  (insert "|-")
  (newline)
  (cl-loop initially (insert "| * |")
           for (nil . field) in alist
           do (insert (prin1-to-string field t) " | "))
  (org-table-align))

(defun org-srs-table-goto-column (name)
  (when-let ((column (alist-get name (org-srs-table-column-name-number-alist))))
    (org-table-goto-column column) t))

(defun org-srs-table-field (&optional column)
  (cl-assert (org-srs-table-goto-column column))
  (org-table-get nil nil))

(defun \(setf\ org-srs-table-field\) (value &optional column)
  (if column
      (progn
        (cl-assert (org-srs-table-goto-column column))
        (setf (org-srs-table-field) value))
    (org-table-blank-field)
    (cl-assert (looking-at (rx (+ blank))))
    (let* ((value-length (length value))
           (field-length (- (match-end 0) (match-beginning 0))))
      (cl-assert (= (point) (match-beginning 0)))
      (when (> field-length value-length)
        (delete-region (point) (+ (point) value-length))))
    (insert value)))

(cl-defun org-srs-table-call-with-temp-buffer-1 (thunk
                                                 &optional
                                                 (table (buffer-substring-no-properties
                                                         (org-srs-table-begin)
                                                         (org-srs-table-end)))
                                                 (point (- (point) (org-srs-table-begin))))
  (with-temp-buffer
    (insert table)
    (let ((org-mode-hook nil)) (org-mode))
    (goto-char (+ (org-srs-table-begin) point))
    (funcall thunk)
    (let* ((begin (org-srs-table-begin)) (end (org-table-end)) (point (- (point) begin)))
      (cl-values (buffer-substring-no-properties begin end) point))))

(cl-defmacro org-srs-table-with-temp-buffer-1 (&rest body)
  (declare (indent 0))
  `(org-srs-table-call-with-temp-buffer-1 (lambda () . ,body)))

(defun org-srs-table-call-with-temp-buffer (thunk)
  (let* ((begin (org-srs-table-begin)) (end (org-srs-table-end)) (point (- (point) begin))
         (table (buffer-substring-no-properties begin end)))
    (cl-multiple-value-bind (table point) (org-srs-table-call-with-temp-buffer-1 thunk table point)
      (delete-region begin end)
      (insert table)
      (goto-char (+ begin point)))))

(cl-defmacro org-srs-table-with-temp-buffer (&rest body)
  (declare (indent 0))
  `(org-srs-table-call-with-temp-buffer (lambda () . ,body)))

(defvar org-table-get-stored-formulas@org-table-formula-named-column-lhs-support nil)

(define-advice org-table-get-stored-formulas (:around (fun &rest args) org-table-formula-named-column-lhs-support)
  (let ((org-table-get-stored-formulas@org-table-formula-named-column-lhs-support t))
    (apply fun args)))

(define-advice org-split-string (:filter-return (formulas) org-table-formula-named-column-lhs-support)
  (if org-table-get-stored-formulas@org-table-formula-named-column-lhs-support
      (mapcar
       (lambda (formula)
         (cl-loop for string = formula then (if column (replace-match column t t string 1) string)
                  for end = 0 then (1+ start)
                  for start = (or (string-match (rx "$" (group (+? (not blank))) (or "=" "..")) string end) (cl-return string))
                  for name = (match-string 1 string)
                  for column = (alist-get name org-table-column-names nil nil #'string-equal)))
       formulas)
    formulas))

(provide 'org-srs-table)
;;; org-srs-table.el ends here
