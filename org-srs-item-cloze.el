;;; org-srs-item-cloze.el --- Cloze deletion item type -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Bohong Huang

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

;; This package implements cloze deletion review items in Org-srs,
;; supporting hints, position-independent identifiers, and batch
;; (un)clozing.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)
(require 'custom)

(require 'org)
(require 'org-element)

(require 'org-srs-property)
(require 'org-srs-entry)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-query)

(defgroup org-srs-item-cloze nil
  "Cloze type for review items."
  :group 'org-srs-item
  :prefix "org-srs-item-cloze-")

(defconst org-srs-item-cloze-regexp
  (rx "{{" (group (*? not-newline))
      "}{" (group (*? not-newline))
      (or "}}" (and "}{" (group (*? not-newline)) "}}")))
  "Regular expression for matching the cloze deletion syntax of Org-srs.")

(defun org-srs-item-cloze-bounds-1-regexp ()
  "Match the bounds of the cloze deletion using regular expression."
  (cl-values-list
   (cl-list*
    (cons (match-beginning 0) (match-end 0))
    (cons (match-beginning 1) (match-end 1))
    (cons (match-beginning 2) (match-end 2))
    (when (and (match-beginning 3) (match-end 3))
      (list (cons (match-beginning 3) (match-end 3)))))))

(defmacro org-srs-item-cloze-define-peg-parser ()
  "Define the PEG parser matching cloze deletions if library `peg' is available."
  (when (require 'peg nil t)
    `(progn
       (require 'peg)
       (define-peg-ruleset org-srs-item-cloze
         (text () (* (or (and (or "\\" (not ["{}"])) (any)) (nested (peg text)))))
         (nested (content) (and "{" (funcall content) "}"))
         (cloze () (nested (peg (and #1=(nested (peg (region (text)))) #1# (opt #1#))))
                `(v1 v2 v3 v4 v5 v6 --
                     (cl-loop for (start end) on (list v1 v2 v3 v4 v5 v6) by #'cddr
                              when (and start end) collect (cons start end)))))
       (defun org-srs-item-cloze-bounds-1-peg ()
         "Match the bounds of the cloze deletion using PEG."
         (ignore-error peg-search-failed
           (with-peg-rules (org-srs-item-cloze)
             (let ((result (car (peg-parse cloze))))
               (cl-values-list
                (cons (cons (- (car (cl-first result)) 2)
                            (+ (cdar (last result)) 2))
                      result)))))))))

(org-srs-item-cloze-define-peg-parser)

(defun org-srs-item-cloze-bounds-1 ()
  "Match the bounds of the cloze deletion using PEG or regular expression."
  (if-let ((fun (symbol-function 'org-srs-item-cloze-bounds-1-peg)))
      (funcall fun)
    (org-srs-item-cloze-bounds-1-regexp)))

(cl-defun org-srs-item-cloze-collect-1 (&optional (start (point-min)) (end (point-max)))
  "Collect cloze deletion bounds between START and END."
  (save-excursion
    (cl-loop initially (goto-char start)
             for foundp = (re-search-forward org-srs-item-cloze-regexp end t)
             for bound-start = (goto-char (match-beginning 0))
             for bound-end = (match-end 0)
             for bounds = (when foundp (cl-multiple-value-list (org-srs-item-cloze-bounds-1)))
             while foundp
             if bounds collect bounds and do (goto-char bound-end)
             else do (goto-char (1+ bound-start)))))

(cl-defun org-srs-item-cloze-collect (&optional (start (point-min)) (end (point-max)))
  "Collect cloze deletion data between START and END."
  (cl-loop for (bounds id-bounds text-bounds hint-bounds) in (org-srs-item-cloze-collect-1 start end)
           collect (cl-list*
                    (read (buffer-substring-no-properties (car id-bounds) (cdr id-bounds)))
                    (car bounds) (cdr bounds)
                    (buffer-substring (car text-bounds) (cdr text-bounds))
                    (when hint-bounds (list (buffer-substring (car hint-bounds) (cdr hint-bounds)))))))

(org-srs-property-defcustom org-srs-item-cloze-visibility nil
  "Visibility of cloze deletions other than the one currently being reviewed."
  :group 'org-srs-item-cloze
  :type 'boolean)

(defun org-srs-item-cloze-string-pad-width (string width)
  "Pad STRING with spaces to make it WIDTH characters wide."
  (concat string (make-string (- width (string-width string)) ? )))

(defun org-srs-item-cloze-pom-at-table-p (point-or-marker)
  "Return non-nil if POINT-OR-MARKER is inside an Org table."
  (save-excursion
    (goto-char point-or-marker)
    (org-at-table-p)))

(defun org-srs-item-cloze-overlay-text (overlay)
  "Return the display text property from OVERLAY."
  (overlay-get overlay 'display))

(cl-defmethod (setf org-srs-item-cloze-overlay-text) (text overlay)
  "Set the display text of OVERLAY to TEXT."
  (overlay-put
   overlay 'display
   (if (org-srs-item-cloze-pom-at-table-p (overlay-start overlay))
       (org-srs-item-cloze-string-pad-width
        text (string-width (buffer-substring (overlay-start overlay) (overlay-end overlay))))
     text)))

(cl-defun org-srs-item-cloze-put-overlay (start end &optional (text ""))
  "Create a new cloze overlay from START to END with TEXT."
  (cl-check-type text string)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'category 'org-srs-item-cloze)
    (setf (org-srs-item-cloze-overlay-text overlay) text)
    overlay))

(cl-defun org-srs-item-cloze-remove-overlays (&optional
                                              (start (org-srs-entry-beginning-position))
                                              (end (org-srs-entry-end-position)))
  "Remove all cloze overlays between START and END."
  (remove-overlays start end 'category 'org-srs-item-cloze))

(defun org-srs-item-cloze-current (&optional hint)
  "Return the text displayed in the overlay of a cloze deletion being answered.

HINT is an optional string shown between brackets to help with the answer."
  (concat
   (propertize "[..." 'face 'bold)
   (or hint "")
   (propertize "]" 'face 'bold)))

(defun org-srs-item-cloze-hidden (&optional text)
  "Return the text displayed in overlays of cloze deletions not being answered.

TEXT is an optional string shown between brackets to indicate hidden content."
  (concat
   (propertize "[" 'face 'bold)
   (or text "   ")
   (propertize "]" 'face 'bold)))

(defun org-srs-item-cloze-answer (text)
  "Return the text showing the answer in the overlay of a cloze deletion.

TEXT is the string containing the answer to be shown between brackets."
  (concat
   (propertize "[" 'face 'bold)
   text
   (propertize "]" 'face 'bold)))

(defun org-srs-item-cloze-recenter-horizontally ()
  "Adjust the window's horizontal scroll to center the point position."
  (let ((offset (- (current-column) (truncate (window-width) 2))))
    (set-window-hscroll (selected-window) (max offset 0))))

(org-srs-property-defcustom org-srs-item-cloze-centered-in-review-p nil
  "Non-nil means the cloze deletion in review will be centered in selected window."
  :group 'org-srs-item-cloze
  :type '(choice boolean function))

(cl-defmethod org-srs-item-review ((type (eql 'cloze)) &rest args)
  "Method to review an item of TYPE `cloze' with ARGS."
  (cl-loop with visibility = (org-srs-item-cloze-visibility) and cloze-id-set = args
           initially (org-srs-item-cloze-remove-overlays)
           for cloze in (progn (org-srs-item-narrow) (org-srs-item-cloze-collect))
           for (id . (start end text hint)) = cloze
           if (or (null cloze-id-set) (member id cloze-id-set))
           collect (cons (org-srs-item-cloze-put-overlay
                          start end
                          (org-srs-item-cloze-current hint))
                         cloze)
           into hidden-clozes
           else
           do (cl-ecase visibility
                ((nil) (org-srs-item-cloze-put-overlay start end (org-srs-item-cloze-hidden)))
                ((t) (org-srs-item-cloze-put-overlay start end text)))
           finally
           (cl-loop with centeredp = (org-srs-item-cloze-centered-in-review-p)
                    for (overlay _id start _end text _hint) in hidden-clozes
                    do (cl-assert (overlayp overlay))
                    unless (> (length hidden-clozes) 1)
                    do (goto-char start)
                    and when (cl-etypecase centeredp
                               (boolean centeredp)
                               (function (funcall centeredp)))
                    do (recenter) (org-srs-item-cloze-recenter-horizontally)
                    do (org-srs-item-add-hook-once
                        'org-srs-item-after-confirm-hook
                        (apply-partially #'\(setf\ org-srs-item-cloze-overlay-text\) (org-srs-item-cloze-answer text) overlay)))
           (org-srs-item-add-hook-once 'org-srs-review-continue-hook #'org-srs-item-cloze-remove-overlays 50)
           (apply (org-srs-item-confirm) type args)))

(defun org-srs-item-cloze-identifier-sha1sum-short (content)
  "Return a short SHA1 hash of CONTENT as an interned symbol identifier."
  (intern (substring-no-properties (sha1 content) 0 7)))

(defun org-srs-item-cloze-identifier-number-sequence (&optional _content)
  "Generate a sequence number identifier for the next cloze deletion."
  (cl-loop with (before . after) = (or (cl-nth-value 0 (org-srs-item-cloze-bounds)) (cons (point) (point)))
           for expected from 1
           for (actual) in (cl-sort
                            (nconc (org-srs-item-cloze-collect (org-srs-entry-beginning-position) before)
                                   (org-srs-item-cloze-collect after (org-srs-entry-end-position)))
                            #'< :key #'car)
           while (= actual expected)
           finally (cl-return expected)))

(org-srs-property-defcustom org-srs-item-cloze-identifier #'org-srs-item-cloze-identifier-sha1sum-short
  "Identifier type used to distinguish cloze deletions."
  :group 'org-srs-item-cloze
  :type 'function)

(defun org-srs-item-cloze-default (start end &optional hint)
  "Create a cloze deletion between START and END with optional HINT."
  (save-excursion
    (goto-char end)
    (if hint (insert "}{" hint "}}") (insert "}}"))
    (goto-char start)
    (insert
     "{{"
     (org-srs-item-princ-to-string
      (funcall (org-srs-item-cloze-identifier) (buffer-substring start end)))
     "}{")))

(defvar org-srs-item-cloze-hint nil
  "Default hint text used when creating a cloze deletion.")

(defvar org-srs-item-cloze-function #'org-srs-item-cloze-default
  "Function to create a new cloze deletion.")

(cl-defgeneric org-srs-item-cloze (type &optional props)
  (:method
   ((type (eql 'paragraph)) &optional props)
   "Method to create a cloze deletion for an element of TYPE `paragraph' with PROPS."
   (let* ((element (list type props))
          (start (org-element-begin element))
          (end (org-element-end element)))
     (funcall org-srs-item-cloze-function start end org-srs-item-cloze-hint)))
  (:documentation "Create cloze deletions for the element specified by TYPE and PROPS."))

(cl-defun org-srs-item-cloze-region-element (start end &optional (element (org-element-at-point)))
  "Create a copy of ELEMENT with its boundaries set to START and END."
  (let ((element (org-element-copy element)))
    (setf (org-element-begin element) start
          (org-element-end element) end)
    element))

(cl-defgeneric org-srs-item-cloze-interactively (type &optional props)
  (:method
   (type &optional props)
   "Default method to interactively create a cloze deletion.

TYPE and PROPS are passed to `org-srs-item-cloze' as is."
   (org-srs-item-cloze type props))
  (:method
   (type &context ((region-active-p) (eql t)) &optional props)
   "Method to create a cloze deletion interactively when REGION-ACTIVE-P is t."
   (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element (region-beginning) (region-end) (list type props)))))
  (:method
   (type &context ((region-active-p) (eql nil)) ((bounds-of-thing-at-point 'word) cons) &optional props)
   "Method to create a cloze deletion interactively when REGION-ACTIVE-P is nil.

BOUNDS-OF-THING-AT-POINT called with `word' must return a cons cell to ensure a
word is at point."
   (cl-destructuring-bind (start . end) (bounds-of-thing-at-point 'word)
     (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element start end (list type props))))))
  (:documentation "Interactively make cloze deletions for the element specified by TYPE and PROPS."))

;;;###autoload
(defun org-srs-item-cloze-dwim ()
  "Create cloze deletions at point.

If there is an active region, cloze the region.
If point is on a word, cloze that word.
If point is at the beginning of a table, perform batch clozing on the table's
fields."
  (interactive)
  (apply #'org-srs-item-cloze-interactively (org-element-at-point)))

(cl-defun org-srs-item-cloze-bounds (&optional (position (point)))
  "Return the bounds of the cloze deletion at POSITION as multiple values."
  (save-excursion
    (cl-loop for bounds in (progn (goto-char position) (org-srs-item-cloze-collect-1 (line-beginning-position) (line-end-position)))
             for ((start . end)) = bounds
             when (<= start position (1- end))
             return (cl-values-list bounds))))

(defun org-srs-item-uncloze-default (start end)
  "Remove all cloze deletions between START and END."
  (save-excursion
    (cl-loop for ((start . end) nil (text-start . text-end)) in (nreverse (org-srs-item-cloze-collect-1 start end))
             do (delete-region text-end end) (delete-region start text-start))))

(defvar org-srs-item-uncloze-function #'org-srs-item-uncloze-default
  "Function used to remove cloze deletions from a region.")

(cl-defgeneric org-srs-item-uncloze (type &optional props)
  (:method
   (type &optional props)
   "Default method to delete cloze deletions for an element of TYPE with PROPS."
   (let* ((element (list type props))
          (start (org-element-begin element))
          (end (org-element-end element)))
     (funcall org-srs-item-uncloze-function start end)))
  (:documentation "Delete cloze deletions for the element specified by TYPE and PROPS."))

(cl-defgeneric org-srs-item-uncloze-interactively (type &optional props)
  (:method
   (type &optional props)
   "Default method to interactively delete a cloze deletion.

TYPE and PROPS are passed to `org-srs-item-uncloze' as is."
   (org-srs-item-uncloze type props))
  (:method
   (type &context ((region-active-p) (eql t)) &optional props)
   "Method to delete cloze deletions interactively when REGION-ACTIVE-P is t.

TYPE and PROPS are passed to `org-srs-item-uncloze' as is."
   (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element (region-beginning) (region-end) (list type props)))))
  (:method
   (type &context ((region-active-p) (eql nil)) ((cl-nth-value 0 (org-srs-item-cloze-bounds)) cons) &optional props)
   "Method to delete cloze deletions interactively when REGION-ACTIVE-P is nil.

BOUNDS-OF-THING-AT-POINT called with `word' must return a cons cell to ensure a
word is at point."
   (cl-destructuring-bind (start . end) (cl-nth-value 0 (org-srs-item-cloze-bounds))
     (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element start end (list type props))))))
  (:documentation "Interactively delete cloze deletions in the element specified by TYPE and PROPS."))

;;;###autoload
(defun org-srs-item-uncloze-dwim ()
  "Delete cloze deletions at point. Also see `org-srs-item-cloze-dwim'."
  (interactive)
  (apply #'org-srs-item-uncloze-interactively (org-element-at-point)))

(cl-defun org-srs-item-cloze-remove-duplicates (list &key (key #'identity) (test #'eql))
  "Like `cl-remove-duplicates', but preserve each element's first occurrence order.

KEY is a function used to extract keys for comparison.
TEST is a function used to compare keys for equality.

Return a copy of LIST with all duplicate elements removed."
  (cl-loop for elem in list
           unless (cl-member (funcall key elem) result :test test :key key)
           collect elem into result
           finally (cl-return result)))

(defun org-srs-item-cloze-update-entry (&optional map-history-p)
  "Update review items for cloze deletions in the current entry.

When MAP-HISTORY-P is non-nil, the review histories of original cloze deletions
will be inherited based on their positions sequentially."
  (let* ((start (org-srs-entry-beginning-position))
         (end (org-srs-entry-end-position))
         (items (cl-delete 'cloze (cl-mapcar #'cl-first (org-srs-query '(and) (cons start end))) :key #'car :test-not #'eq))
         (clozes (org-srs-item-cloze-remove-duplicates (org-srs-item-cloze-collect start end) :key #'cl-first :test #'eq)))
    (setf map-history-p (if (= (length items) (length clozes))
                            (unless (equal (mapcar #'cl-second items) (mapcar #'cl-first clozes))
                              (cl-etypecase map-history-p
                                (boolean map-history-p)
                                (function (funcall map-history-p))))
                          (cl-assert (not (eq map-history-p t)))))
    (if map-history-p
        (cl-loop for item in items
                 for marker in (mapcar #'org-srs-item-marker items)
                 for (identifier) in clozes
                 do
                 (org-srs-item-goto-marker marker)
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
  (cl-flet ((update-cloze (&optional (bounds (cl-nth-value 0 (org-srs-item-cloze-bounds))))
              (cl-destructuring-bind (start . end) bounds
                (goto-char start)
                (cl-assert (looking-at org-srs-item-cloze-regexp))
                (let ((content (match-string 2)))
                  (replace-match (save-match-data (org-srs-item-princ-to-string (funcall (org-srs-item-cloze-identifier) content))) t t nil 1)))))
    (save-excursion
      (org-id-get-create)
      (if (<= arg 1)
          (if-let ((bounds (cl-nth-value 0 (org-srs-item-cloze-bounds))))
              (progn (update-cloze bounds) (org-srs-item-cloze-update-entry t))
            (org-srs-item-cloze-update-entry
             (when (called-interactively-p 'interactive)
               (apply-partially #'y-or-n-p "Positionally inherit the review histories of cloze deletions?"))))
        (cl-loop initially (org-srs-item-cloze-update-entry nil)
                 for bounds in (cl-loop for (nil start end) in (org-srs-item-cloze-collect (org-srs-entry-beginning-position) (org-srs-entry-end-position))
                                        collect (cons (copy-marker start) (copy-marker end)))
                 do (update-cloze bounds)
                 finally (org-srs-item-cloze-update-entry t)))
      (org-srs-log-hide-drawer))))

(org-srs-property-defcustom org-srs-item-cloze-table-range "@<<$<<..@>$>"
  "Default table range expression used for batch clozing."
  :group 'org-srs-item-cloze
  :type 'string)

(cl-defun org-srs-item-cloze-table-fields (&optional (range '(1 . 1)))
  "Create cloze deletions for fields in an Org table within RANGE."
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
                           (cl-assert (looking-back (rx (literal field)) (line-beginning-position)))
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
  "Method to create cloze deletions for an element of TYPE `table' with PROPS."
  (let ((element (list type props)))
    (cl-assert (<= (org-element-begin element) (point) (1- (org-element-end element)))))
  (org-srs-item-cloze-table-fields (org-srs-item-cloze-table-range)))

(cl-defmethod org-srs-item-cloze-interactively ((type (eql 'table)) &optional props)
  "Method to interactively create cloze deletions for an element of TYPE `table'.

TYPE and PROPS are passed to `org-srs-item-cloze' as is."
  (org-srs-property-let ((org-srs-item-cloze-table-range (read-string "Range: " (org-srs-item-cloze-table-range))))
    (org-srs-item-cloze type props)))

(cl-defmethod org-srs-item-uncloze ((type (eql 'table)) &optional props)
  "Method to delete cloze deletions for an element of TYPE `table' with PROPS."
  (cl-call-next-method)
  (let ((element (list type props)))
    (cl-assert (<= (org-element-begin element) (point) (1- (org-element-end element)))))
  (org-table-align))

(defun org-srs-item-cloze-item-at-point ()
  "Return the review item corresponding to the cloze deletion at point."
  (save-excursion
    (goto-char (car (cl-nth-value 0 (org-srs-item-cloze-bounds))))
    (cl-assert (looking-at org-srs-item-cloze-regexp))
    (list 'cloze (read (match-string 1)))))

(cl-defmethod org-srs-item-new-interactively ((_type (eql 'cloze)) &rest args)
  "Method for interactively creating a new review item of type `cloze' with ARGS."
  (if (null args)
      (if (region-active-p)
          (if-let ((clozes (org-srs-item-cloze-collect (region-beginning) (region-end))))
              (org-srs-item-new (cons 'cloze (mapcar #'cl-first clozes)))
            (org-srs-item-cloze-dwim)
            (deactivate-mark)
            (org-srs-item-new (org-srs-item-cloze-item-at-point)))
        (org-srs-item-new (or 'cloze (org-srs-item-cloze-item-at-point))))
    (cl-call-next-method)))

(provide 'org-srs-item-cloze)
;;; org-srs-item-cloze.el ends here
