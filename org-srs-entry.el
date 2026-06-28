;;; org-srs-entry.el --- Operations related to review entries -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Bohong Huang

;; Author: Bohong Huang <bohonghuang@qq.com>
;; Maintainer: Bohong Huang <bohonghuang@qq.com>
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (org "9.7") (fsrs "6.0"))
;; URL: https://github.com/bohonghuang/org-srs
;; Keywords: outlines

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

;; This package provides operations related to review entries in Org-srs.

;;; Code:

(require 'cl-lib)
(require 'outline)

(require 'org)
(require 'org-element)

(defun org-srs-entry-beginning-position ()
  "Return the beginning position of the current entry or document."
  (if (org-before-first-heading-p)
      (point-min)
    (org-entry-beginning-position)))

(defun org-srs-entry-end-position (&optional subtree)
  "Return the end position of the current entry or document preface.

If SUBTREE is non-nil, return the end position of the current subtree
or document."
  (let ((position (if (org-before-first-heading-p)
                      (if subtree
                          (point-max)
                        (save-excursion (outline-next-heading) (point)))
                    (if subtree
                        (save-excursion
                          (org-end-of-subtree nil t))
                      (org-entry-end-position)))))
    (if (< position (point-max)) (1- position) position)))

(defalias 'org-srs-entry-subtree-beginning-position #'org-srs-entry-beginning-position
  "Return the beginning position of the current subtree or document.")

(defalias 'org-srs-entry-subtree-end-position (apply-partially #'org-srs-entry-end-position t)
  "Return the end position of the current subtree or document.")

(define-advice org-back-to-heading (:around (fun &rest args) org-srs-entry-end-of-meta-data)
  (if (bound-and-true-p org-srs-entry-end-of-meta-data)
      (progn
        (goto-char (point-min))
        (let ((end (save-excursion (outline-next-heading) (point))))
          (when (re-search-forward org-property-drawer-re end t)
            (goto-char (match-end 0))
            (beginning-of-line)
            (when (eq org-srs-entry-end-of-meta-data t)
              (setf org-srs-entry-end-of-meta-data (point))))))
    (apply fun args)))

(defun org-srs-entry-end-of-meta-data (&rest args)
  "Like `org-end-of-meta-data', but can be used outside of headings.

ARGS are passed to `org-end-of-meta-data' as is."
  (defvar org-srs-entry-end-of-meta-data)
  (if (org-before-first-heading-p)
      (let ((org-srs-entry-end-of-meta-data t))
        (prog1 (apply #'org-end-of-meta-data args)
          (when (eq org-srs-entry-end-of-meta-data t)
            (forward-line -1))))
    (apply #'org-end-of-meta-data args)))

(defun org-srs-entry-end-of-drawer (name)
  "Move point to the end of the drawer named NAME, creating it if necessary."
  (save-restriction
    (org-back-to-heading-or-point-min)
    (let ((entry-start (point))
          (entry-end (org-srs-entry-end-position))
          (drawer-start-regexp (rx bol (* blank) ":" (literal name) ":" (* blank) eol))
          (drawer-end-regexp (rx bol (* blank) ":END:" (* blank) eol)))
      (if (re-search-forward drawer-start-regexp entry-end t)
          (progn
            (goto-char (org-element-end (org-element-at-point)))
            (re-search-backward drawer-end-regexp entry-start))
        (org-srs-entry-end-of-meta-data t)
        (unless (re-search-backward drawer-end-regexp entry-start t)
          (org-back-to-heading-or-point-min))
        (end-of-line)
        (newline-and-indent)
        (insert ":" name ":")
        (newline-and-indent)
        (insert ":END:")
        (beginning-of-line)))))

(defun org-srs-entry-beginning-of-drawer (name)
  "Move point to the beginning of the drawer named NAME, creating it if necessary."
  (org-back-to-heading-or-point-min)
  (let ((heading-start (point)))
    (org-srs-entry-end-of-drawer name)
    (re-search-backward (rx bol (* blank) ":" (literal name) ":" (* blank) eol) heading-start)))

(provide 'org-srs-entry)
;;; org-srs-entry.el ends here
