;;; org-srs-item-cloze.el --- The cloze deletion item type -*- lexical-binding:t -*-

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

;; This package implements cloze deletion review items in Org-srs,
;; supporting hints, position-independent identifiers, and batch
;; (un)clozing.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)

(require 'org)
(require 'org-element)

(require 'org-srs-property)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-query)

(defconst org-srs-item-cloze-regexp
  (rx "{{" (group (*? not-newline))
      "}{" (group (*? not-newline))
      (or "}}" (and "}{" (group (*? not-newline)) "}}"))))

(cl-defun org-srs-item-cloze-collect (&optional (start (point-min)) (end (point-max)))
  (save-excursion
    (goto-char start)
    (cl-loop while (re-search-forward org-srs-item-cloze-regexp end t)
             collect (cl-list*
                      (read (match-string-no-properties 1)) (match-beginning 0)
                      (match-end 0) (match-string 2)
                      (when-let ((hint (match-string 3))) (list hint))))))

(org-srs-property-defcustom org-srs-item-cloze-visibility nil
  "The visibility of cloze deletions other than the one currently being reviewed."
  :group 'org-srs
  :type 'boolean)

(defun org-srs-item-cloze-string-pad-width (string width)
  (concat string (make-string (- width (string-width string)) ? )))

(defun org-srs-item-cloze-pom-at-table-p (point-or-marker)
  (save-excursion
    (goto-char point-or-marker)
    (org-at-table-p)))

(defun org-srs-item-cloze-overlay-text (overlay)
  (overlay-get overlay 'display))

(defun \(setf\ org-srs-item-cloze-overlay-text\) (text overlay)
  (overlay-put
   overlay 'display
   (if (org-srs-item-cloze-pom-at-table-p (overlay-start overlay))
       (org-srs-item-cloze-string-pad-width
        text (string-width (buffer-substring (overlay-start overlay) (overlay-end overlay))))
     text)))

(cl-defun org-srs-item-cloze-put-overlay (start end &optional (text ""))
  (cl-check-type text string)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'category 'org-srs-item-cloze)
    (setf (org-srs-item-cloze-overlay-text overlay) text)
    overlay))

(cl-defun org-srs-item-cloze-remove-overlays (&optional
                                              (start (org-entry-beginning-position))
                                              (end (org-entry-end-position)))
  (remove-overlays start end 'category 'org-srs-item-cloze))

(defun org-srs-item-cloze-current (&optional hint)
  (concat
   (propertize "[..." 'face 'bold)
   (or hint "")
   (propertize "]" 'face 'bold)))

(defun org-srs-item-cloze-hidden (&optional text)
  (concat
   (propertize "[" 'face 'bold)
   (or text "   ")
   (propertize "]" 'face 'bold)))

(defun org-srs-item-cloze-answer (text)
  (concat
   (propertize "[" 'face 'bold)
   text
   (propertize "]" 'face 'bold)))

(defun org-srs-item-cloze-recenter-horizontally ()
  (let ((offset (- (current-column) (truncate (window-width) 2))))
    (set-window-hscroll (selected-window) (max offset 0))))

(org-srs-property-defcustom org-srs-item-cloze-centered-in-review-p nil
  "Non-nil means the cloze deletion in review will be centered in selected window."
  :group 'org-srs
  :type 'boolean)

(cl-defmethod org-srs-item-review ((type (eql 'cloze)) &rest args)
  (cl-loop with visibility = (org-srs-item-cloze-visibility) and current
           with (cloze-id) = args
           initially (org-srs-item-cloze-remove-overlays)
           for cloze in (progn (org-srs-item-narrow) (org-srs-item-cloze-collect))
           for (id . (start end text hint)) = cloze
           if (eql id cloze-id)
           do (setf current (cons (org-srs-item-cloze-put-overlay
                                   start end
                                   (org-srs-item-cloze-current hint))
                                  cloze))
           else
           do (cl-ecase visibility
                ((nil) (org-srs-item-cloze-put-overlay start end (org-srs-item-cloze-hidden)))
                ((t) (org-srs-item-cloze-put-overlay start end text)))
           finally
           (org-srs-item-add-hook-once
            'org-srs-item-after-confirm-hook
            (cl-destructuring-bind (overlay _id start _end text &optional _hint) current
              (cl-assert (overlayp overlay))
              (when (org-srs-item-cloze-centered-in-review-p)
                (goto-char start)
                (recenter)
                (org-srs-item-cloze-recenter-horizontally))
              (lambda ()
                (setf (org-srs-item-cloze-overlay-text overlay) (org-srs-item-cloze-answer text)))))
           (org-srs-item-add-hook-once 'org-srs-review-after-rate-hook #'org-srs-item-cloze-remove-overlays)
           (apply (org-srs-item-confirmation) type args)))

(defun org-srs-item-cloze-sha1sum-short (content)
  (intern (substring (sha1 content) 0 7)))

(org-srs-property-defcustom org-srs-item-cloze-identifier #'org-srs-item-cloze-sha1sum-short
  "The identifier type used to distinguish cloze deletions."
  :group 'org-srs
  :type 'function)

(defun org-srs-item-cloze-default (start end &optional hint)
  (let ((identifier (funcall (org-srs-item-cloze-identifier) (buffer-substring-no-properties start end))))
    (save-excursion
      (goto-char end)
      (if hint (insert "}{" hint "}}") (insert "}}"))
      (goto-char start)
      (insert "{{" (org-srs-item-princ-to-string identifier) "}{"))))

(defvar org-srs-item-cloze-hint nil)

(defvar org-srs-item-cloze-function #'org-srs-item-cloze-default)

(cl-defgeneric org-srs-item-cloze (type &optional props)
  (:method
   ((type (eql 'paragraph)) &optional props)
   (let* ((element (list type props))
          (start (org-element-begin element))
          (end (org-element-end element)))
     (funcall org-srs-item-cloze-function start end org-srs-item-cloze-hint))))

(cl-defun org-srs-item-cloze-region-element (start end &optional (element (org-element-at-point)))
  (let ((element (org-element-copy element)))
    (setf (org-element-begin element) start
          (org-element-end element) end)
    element))

(cl-defgeneric org-srs-item-cloze-interactively (type &optional props)
  (:method
   (type &optional props)
   (org-srs-item-cloze type props))
  (:method
   (type &context ((region-active-p) (eql t)) &optional props)
   (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element (region-beginning) (region-end) (list type props)))))
  (:method
   (type &context ((region-active-p) (eql nil)) ((bounds-of-thing-at-point 'word) cons) &optional props)
   (cl-destructuring-bind (start . end) (bounds-of-thing-at-point 'word)
     (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element start end (list type props)))))))

;;;###autoload
(defun org-srs-item-cloze-dwim ()
  "Cloze the element at point.

If there is an active region, cloze the region.
If point is on a word, cloze that word.
If point is at the beginning of a table, perform batch clozing on the table's
fields."
  (interactive)
  (apply #'org-srs-item-cloze-interactively (org-element-at-point)))

(cl-defun org-srs-item-cloze-bounds (&optional (position (point)))
  (save-excursion
    (cl-loop for function in '(re-search-backward re-search-forward)
             for match-bound in '(match-end match-beginning)
             for line-bound in (list (pos-bol) (pos-eol))
             if (funcall function org-srs-item-cloze-regexp line-bound t)
             if (<= (match-beginning 0) position (1- (match-end 0)))
             return (cons (match-beginning 0) (match-end 0))
             else do (goto-char (funcall match-bound 0))
             else do (goto-char line-bound))))

(cl-defun org-srs-item-uncloze-default (start end)
  (save-excursion
    (cl-loop initially (goto-char start)
             while (re-search-forward org-srs-item-cloze-regexp end t)
             do (replace-match "\\2" t))))

(defvar org-srs-item-uncloze-function #'org-srs-item-uncloze-default)

(cl-defgeneric org-srs-item-uncloze (type &optional props)
  (let* ((element (list type props))
         (start (org-element-begin element))
         (end (org-element-end element)))
    (funcall org-srs-item-uncloze-function start end)))

(cl-defgeneric org-srs-item-uncloze-interactively (type &optional props)
  (:method
   (type &optional props)
   (org-srs-item-uncloze type props))
  (:method
   (type &context ((region-active-p) (eql t)) &optional props)
   (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element (region-beginning) (region-end) (list type props)))))
  (:method
   (type &context ((region-active-p) (eql nil)) ((org-srs-item-cloze-bounds) cons) &optional props)
   (cl-destructuring-bind (start . end) (org-srs-item-cloze-bounds)
     (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element start end (list type props)))))))

;;;###autoload
(defun org-srs-item-uncloze-dwim ()
  "Uncloze the element at point. Also see `org-srs-item-cloze-dwim'."
  (interactive)
  (apply #'org-srs-item-uncloze-interactively (org-element-at-point)))

(defun org-srs-item-cloze-update-entry (&optional inherit-history-p)
  (let* ((start (org-entry-beginning-position))
         (end (org-entry-end-position))
         (items (cl-delete 'cloze (cl-mapcar #'cl-first (org-srs-query-region (org-srs-query-predicate-and) start end)) :key #'car :test-not #'eq))
         (clozes (org-srs-item-cloze-collect start end)))
    (setf inherit-history-p (if (= (length items) (length clozes))
                                (unless (equal (mapcar #'cl-second items) (mapcar #'cl-first clozes))
                                  (cl-etypecase inherit-history-p
                                    (boolean inherit-history-p)
                                    (function (funcall inherit-history-p))))
                              (cl-assert (not (eq inherit-history-p t)))))
    (if inherit-history-p
        (cl-loop for item in items
                 for (identifier) in clozes
                 do
                 (org-srs-item-goto item)
                 (beginning-of-line)
                 (cl-assert (looking-at org-srs-item-header-regexp))
                 (replace-match
                  (cl-reduce
                   (lambda (acc it) (format "%s::%s" acc (org-srs-item-princ-to-string it)))
                   (list (car item) identifier))
                  t t nil 2))
      (let ((history-alist (cl-loop for item in items
                                    for (start . end) = (org-srs-item-bounds item)
                                    collect (cons item (buffer-substring start end))
                                    and do (delete-region start end))))
        (cl-loop for (identifier) in clozes
                 for cloze-item = (list 'cloze identifier)
                 for history = (alist-get cloze-item history-alist nil nil #'equal)
                 if history
                 do (org-srs-log-end-of-drawer) (insert history)
                 else
                 do (org-srs-item-new cloze-item))))))

;;;###autoload
(defun org-srs-item-cloze-update (arg)
  "Update the cloze items in the current entry.

By default, this command will not update any identifier, unless
point is inside a cloze deletion.

When called with a `\\[universal-argument]' prefix interactively
or ARG greater than 1, update identifiers and items for all the
cloze deletions in the current entry.

It's recommended to execute this command every time you add,
delete, or modify a cloze deletion."
  (interactive "p")
  (require 'org-srs)
  (cl-flet ((update-cloze (&optional (bounds (org-srs-item-cloze-bounds)))
              (cl-destructuring-bind (start . end) bounds
                (goto-char start)
                (cl-assert (looking-at org-srs-item-cloze-regexp))
                (let ((content (match-string 2)))
                  (replace-match (save-match-data (org-srs-item-princ-to-string (funcall (org-srs-item-cloze-identifier) content))) t t nil 1)))))
    (save-excursion
      (org-id-get-create)
      (if (<= arg 1)
          (if-let ((bounds (org-srs-item-cloze-bounds)))
              (progn (update-cloze bounds) (org-srs-item-cloze-update-entry t))
            (org-srs-item-cloze-update-entry
             (when (called-interactively-p 'interactive)
               (apply-partially #'y-or-n-p "Sequentially inherit review history from before the cloze modification?"))))
        (cl-loop initially (org-srs-item-cloze-update-entry nil)
                 for position in (cl-loop for (nil position) in (org-srs-item-cloze-collect) collect (copy-marker position))
                 do (goto-char position) (update-cloze)
                 finally (org-srs-item-cloze-update-entry t)))
      (org-srs-log-hide-drawer))))

(org-srs-property-defcustom org-srs-item-cloze-table-range "@<<$<<..@>$>"
  "The default table range expression used for batch clozing."
  :group 'org-srs
  :type 'string)

(cl-defun org-srs-item-cloze-table-fields (&optional (range '(1 . 1)))
  (cl-multiple-value-bind (row-start row-end column-start column-end)
      (cl-flet ((ensure-range (object)
                  (cl-typecase object
                    (cons (list (car object) (cdr object)))
                    (fixnum (list object most-positive-fixnum)))))
        (cl-etypecase range
          (cons (cl-values-list (nconc (ensure-range (car range)) (ensure-range (cdr range)))))
          (fixnum (cl-values-list (nconc (ensure-range range) (ensure-range 0))))
          (string
           (string-match
            (rx-let ((ref (or (+ (char "<>")) (+ digit))))
              (rx bos "@" (group ref) "$" (group ref) ".." (optional "@" (group ref) "$" (group ref)) eos))
            range)
           (let ((row-start (match-string 1 range))
                 (column-start (match-string 2 range))
                 (row-end (match-string 3 range))
                 (column-end (match-string 4 range)))
             (cl-multiple-value-bind (rows columns)
                 (cl-loop for line in (org-table-to-lisp)
                          count (listp line) into rows
                          when (listp line)
                          maximize (length line) into columns
                          finally (cl-return (cl-values rows columns)))
               (cl-flet ((parse-ref (desc count)
                           (or (ignore-errors (cl-parse-integer desc))
                               (let ((desc (cl-loop for char across desc sum (cl-case char (?< 1) (?> -1) (t 0)))))
                                 (when (cl-plusp desc) (cl-decf desc))
                                 (mod desc count)))))
                 (cl-values
                  (parse-ref row-start rows)
                  (or (and row-end (parse-ref row-end rows)) (1- rows))
                  (parse-ref column-start columns)
                  (or (and column-end (parse-ref column-end columns)) (1- columns)))))))))
    (save-excursion
      (goto-char (org-table-begin))
      (apply #'org-srs-item-uncloze (org-element-at-point))
      (cl-loop initially (goto-char (org-table-begin))
               for line in (org-table-to-lisp)
               when (listp line)
               do (cl-loop for field in line
                           for column from 0
                           do (org-table-next-field)
                           when (and (<= row-start row row-end) (<= column-start column column-end))
                           unless (string-empty-p (org-table-get nil nil))
                           do
                           (org-table-end-of-field 1)
                           (cl-assert (looking-back (rx (literal field)) (pos-bol)))
                           (let ((start (match-beginning 0))
                                 (end (match-end 0))
                                 (element (org-element-copy (org-element-at-point))))
                             (setf (cl-first element) 'paragraph
                                   (org-element-begin element) start
                                   (org-element-end element) end)
                             (apply #'org-srs-item-cloze element)))
               count (listp line) into row
               finally (org-table-align)))))

(cl-defmethod org-srs-item-cloze ((type (eql 'table)) &optional props)
  (let ((element (list type props)))
    (cl-assert (<= (org-element-begin element) (point) (1- (org-element-end element)))))
  (org-srs-item-cloze-table-fields (org-srs-item-cloze-table-range)))

(cl-defmethod org-srs-item-cloze-interactively ((type (eql 'table)) &optional props)
  (org-srs-property-let ((org-srs-item-cloze-table-range (read-string "Range: " (org-srs-item-cloze-table-range))))
    (org-srs-item-cloze type props)))

(cl-defmethod org-srs-item-uncloze ((type (eql 'table)) &optional props)
  (cl-call-next-method)
  (let ((element (list type props)))
    (cl-assert (<= (org-element-begin element) (point) (1- (org-element-end element)))))
  (org-table-align))

(defun org-srs-item-cloze-item-at-point ()
  (save-excursion
    (goto-char (car (org-srs-item-cloze-bounds)))
    (cl-assert (looking-at org-srs-item-cloze-regexp))
    (list 'cloze (read (match-string 1)))))

(cl-defmethod org-srs-item-new-interactively ((_type (eql 'cloze)) &rest args)
  (if args (cl-call-next-method)
    (org-srs-item-cloze-dwim)
    (deactivate-mark)
    (org-srs-item-new (org-srs-item-cloze-item-at-point))))

(provide 'org-srs-item-cloze)
;;; org-srs-item-cloze.el ends here
