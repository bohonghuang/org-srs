;;; org-srs-log.el --- Operations related to review logs -*- lexical-binding:t -*-

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

;; This package provides operations related to the review log for
;; Org-srs.

;;; Code:

(require 'cl-lib)
(require 'rx)

(require 'org)
(require 'org-element)

(require 'org-srs-algorithm)
(require 'org-srs-time)
(require 'org-srs-table)

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

(defun org-srs-log-insert ()
  (let ((initargs (org-srs-algorithm-repeat (org-srs-algorithm-current) nil)))
    (org-srs-table-from-alist
     `((timestamp . ,(org-srs-timestamp-now)) (rating . ""). ,initargs))
    (org-srs-table-forward-star)
    (setf (org-srs-table-field 'timestamp) (org-srs-timestamp-now))
    (org-table-align)))

(cl-defun org-srs-log-repeat (rating &aux (args nil))
  (org-srs-table-goto-starred-line)
  (forward-line -1)
  (unless (org-at-table-hline-p)
    (setf args (nconc (org-srs-table-current-line) args)))
  (org-srs-table-goto-starred-line)
  (setf (org-srs-table-field 'timestamp) (org-srs-timestamp-now)
        args (nconc (org-srs-table-current-line) args))
  (cl-loop initially (setf args (org-srs-algorithm-repeat (org-srs-algorithm-current) (cl-acons 'rating rating args)))
           for (name . nil) in (org-srs-table-column-name-number-alist)
           for field = (alist-get name args)
           unless (eq name 'timestamp)
           do (setf (org-srs-table-field name) (prin1-to-string field t))
           finally (org-srs-table-forward-star) (setf (org-srs-table-field 'timestamp) (alist-get 'timestamp args)))
  (org-table-align))

(defconst org-srs-log-latest-timestamp-regexp (rx (regexp org-srs-table-starred-line-regexp) (* blank) (group (regexp org-srs-timestamp-regexp))))

(defcustom org-srs-log-drawer-name "SRSITEMS"
  "The name of drawers used to storage items and their review logs."
  :group 'org-srs
  :type 'string)

(defun org-srs-log-end-of-drawer ()
  (save-restriction
    (org-narrow-to-subtree)
    (org-back-to-heading)
    (let ((heading-start (point))
          (drawer-start-regexp (rx bol (* blank) ":" (literal org-srs-log-drawer-name) ":" (* blank) eol))
          (drawer-end-regexp (rx bol (* blank) ":END:" (* blank) eol)))
      (if (re-search-forward drawer-start-regexp nil t)
          (progn
            (goto-char (org-element-end (org-element-at-point)))
            (re-search-backward drawer-end-regexp))
        (org-end-of-meta-data t)
        (unless (re-search-backward drawer-end-regexp heading-start t)
          (org-back-to-heading))
        (end-of-line)
        (newline-and-indent)
        (insert ":" org-srs-log-drawer-name ":")
        (newline-and-indent)
        (insert ":END:")
        (beginning-of-line)))))

(defun org-srs-log-beginning-of-drawer ()
  (save-restriction
    (org-narrow-to-subtree)
    (org-back-to-heading)
    (let ((heading-start (point)))
      (org-srs-log-end-of-drawer)
      (re-search-backward (rx bol (* blank) ":" (literal org-srs-log-drawer-name) ":" (* blank) eol) heading-start))))

(cl-defun org-srs-log-hide-drawer (&optional (position (point)))
  (save-excursion
    (goto-char position)
    (org-srs-log-beginning-of-drawer)
    (org-fold-hide-drawer-toggle t)))

(provide 'org-srs-log)
;;; org-srs-log.el ends here
