;;; org-srs-item-cloze-latex.el --- Support for cloze deletions in LaTeX formulas -*- lexical-binding: t -*-

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

;; This package enables proper display of cloze deletions in LaTeX
;; formulas during reviews, while also ensuring correct rendering without
;; cloze deletion markers of Org-srs outside of review sessions.

;;; Code:

(require 'cl-lib)
(require 'rx)

(require 'org)
(require 'org-element)

(require 'org-srs-entry)
(require 'org-srs-item)
(require 'org-srs-item-cloze)

(cl-defun org-srs-item-cloze-latex-in-preview-p (&optional (position (point)))
  "Return non-nil if POSITION is inside a LaTeX fragment being previewed."
  (and (save-excursion (goto-char position) (org-inside-LaTeX-fragment-p))
       (eq (car (get-char-property position 'display)) 'image)))

(defvar org-srs-item-cloze-latex-display #'cl-fourth
  "Function to derive display texts from results of `org-srs-item-cloze-collect'.")

(defun org-srs-item-cloze-latex-wrapper-delimiter (id)
  "Return a LaTeX wrapper delimiter string for a cloze deletion with ID."
  (concat "{\\iffalse " (format "%s_%s" 'org-srs-item-cloze-latex id) " \\fi}"))

(defun org-srs-item-cloze-latex-wrapper-regexp (id)
  "Return a regular expression matching a LaTeX cloze deletion wrapper with ID."
  (rx (literal (org-srs-item-cloze-latex-wrapper-delimiter id)) (group (*? anychar)) (literal (org-srs-item-cloze-latex-wrapper-delimiter id))))

(defun org-srs-item-cloze-latex-wrap (content id)
  "Wrap CONTENT in a LaTeX cloze deletion wrapper using ID."
  (concat (org-srs-item-cloze-latex-wrapper-delimiter id) content (org-srs-item-cloze-latex-wrapper-delimiter id)))

(cl-defun org-srs-item-cloze-latex-overlays (&optional (start (point)) end)
  "Return a list of LaTeX overlays in the region from START to END."
  (cl-loop for overlay in (if end (overlays-in start end) (overlays-at start))
           when (eq (overlay-get overlay 'org-overlay-type) 'org-latex-overlay)
           collect overlay))

(defun org-srs-item-cloze-latex-process-faces (text)
  "Convert faces in TEXT to their LaTeX equivalents."
  (cl-flet ((bold-verbatim (text)
              (format "\\texttt{\\textbf{%s}}" (replace-regexp-in-string (rx " ") "\\ " text t t))))
    (cl-loop with length = (length text)
             for left = 0 then (if (eq face previous-face) left (1- right))
             for right to length
             for previous-face = (get-text-property 0 'face text) then face
             for face = (when (< right length) (get-text-property right 'face text))
             unless (eq face previous-face)
             concat (funcall
                     (cl-case previous-face
                       (bold #'bold-verbatim)
                       (t #'identity))
                     (substring text left right)))))

(define-advice org--latex-preview-region (:around (fun beg end) org-srs-item-cloze-latex)
  (org-srs-item-with-transient-modifications
    (cl-loop initially (setf beg (copy-marker beg) end (copy-marker end))
             for overlay in (org-srs-item-cloze-latex-overlays beg end)
             for overlay-start = (overlay-start overlay)
             for overlay-end = (overlay-end overlay)
             for content = (overlay-get overlay 'org-srs-item-cloze-latex)
             when content
             do
             (delete-region (1+ overlay-start) (1- overlay-end))
             (goto-char (1+ overlay-start))
             (insert (substring content 1 -1)))
    (cl-loop with position-regexps = nil
             for cloze in (nreverse (org-srs-item-cloze-collect beg end))
             for (cloze-id cloze-start cloze-end) = cloze
             when (and (goto-char cloze-start) (org-inside-LaTeX-fragment-p) (goto-char cloze-end) (org-inside-LaTeX-fragment-p))
             sum 1 into count-total and
             count (when-let ((display (funcall org-srs-item-cloze-latex-display cloze)))
                     (goto-char cloze-end)
                     (insert (org-srs-item-cloze-latex-wrap (org-srs-item-cloze-latex-process-faces display) cloze-id))
                     (delete-region cloze-start cloze-end)
                     (push (cons (copy-marker cloze-start) (org-srs-item-cloze-latex-wrapper-regexp cloze-id)) position-regexps))
             into count-processed
             finally
             (cl-loop initially (when (= count-processed count-total) (org-clear-latex-preview beg end) (funcall fun beg end))
                      for (position . nil) in position-regexps
                      for (overlay) = (org-srs-item-cloze-latex-overlays position)
                      for element = (save-excursion (goto-char position) (org-element-context))
                      for content = (string-trim (buffer-substring (org-element-begin element) (org-element-end element)))
                      do (overlay-put overlay 'modification-hooks nil)
                      unless (eq org-srs-item-cloze-latex-display #'cl-fourth)
                      do (overlay-put overlay 'org-srs-item-cloze-latex content))
             (cl-return position-regexps))))

(define-advice org-srs-item-cloze-put-overlay (:around (fun start end text) org-srs-item-cloze-latex)
  (if (and (org-srs-item-cloze-latex-in-preview-p start) (org-srs-item-cloze-latex-in-preview-p end))
      (cl-destructuring-bind (target-cloze) (org-srs-item-cloze-collect start end)
        (let* ((element (save-excursion (goto-char start) (org-element-context)))
               (position-regexps (let ((org-srs-item-cloze-latex-display
                                        (lambda (cloze)
                                          (unless (and (markerp start) (markerp end))
                                            (when (eq (pop cloze) (cl-first target-cloze))
                                              (setf start (copy-marker (pop cloze))
                                                    end (copy-marker (pop cloze)))
                                              text)))))
                                   (org--latex-preview-region (org-element-begin element) (org-element-end element))))
               (regexp (alist-get start position-regexps nil nil #'=)))
          (cl-check-type regexp string)
          (cons start regexp)))
    (funcall fun start end text)))

(define-advice org-srs-item-cloze-remove-overlays (:after (&rest args) org-srs-item-cloze-latex)
  (cl-destructuring-bind (&optional (start (org-srs-entry-beginning-position)) (end (org-srs-entry-end-position))) args
    (cl-loop for overlay in (org-srs-item-cloze-latex-overlays start end)
             for overlay-start = (overlay-start overlay)
             for overlay-end = (overlay-end overlay)
             when (overlay-get overlay 'org-srs-item-cloze-latex)
             do (org-clear-latex-preview overlay-start overlay-end)
             (org--latex-preview-region overlay-start overlay-end))))

(define-advice org-srs-item-cloze-overlay-text (:around (fun overlay) org-srs-item-cloze-latex)
  (if (consp overlay)
      (cl-destructuring-bind (position . regexp) overlay
        (cl-destructuring-bind (&optional overlay) (org-srs-item-cloze-latex-overlays position)
          (when overlay
            (let ((string (overlay-get overlay 'org-srs-item-cloze-latex)))
              (cl-check-type string string)
              (string-match regexp string)
              (match-string 1 string)))))
    (funcall fun overlay)))

(define-advice \(setf\ org-srs-item-cloze-overlay-text\) (:around (fun text overlay) org-srs-item-cloze-latex)
  (if (consp overlay)
      (cl-destructuring-bind (position . regexp) overlay
        (cl-destructuring-bind (&optional overlay) (org-srs-item-cloze-latex-overlays position)
          (when overlay
            (let ((string (overlay-get overlay 'org-srs-item-cloze-latex))
                  (start (copy-marker (overlay-start overlay)))
                  (end (copy-marker (overlay-end overlay))))
              (cl-check-type string string)
              (org-srs-item-with-transient-modifications
                (org-clear-latex-preview start end)
                (delete-region (1+ start) (1- end))
                (goto-char (1+ start))
                (insert (substring string 1 -1))
                (cl-loop initially (goto-char start)
                         while (re-search-forward regexp end t)
                         do (save-excursion (replace-match (org-srs-item-cloze-latex-process-faces text) t t nil 1)))
                (org--latex-preview-region start end)
                (cl-destructuring-bind (overlay) (org-srs-item-cloze-latex-overlays start end)
                  (overlay-put overlay 'org-srs-item-cloze-latex (buffer-substring start end))
                  (overlay-put overlay 'modification-hooks nil)
                  overlay))))))
    (funcall fun text overlay)))

(provide 'org-srs-item-cloze-latex)
;;; org-srs-item-cloze-latex.el ends here
