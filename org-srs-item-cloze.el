;; -*- lexical-binding: t; -*-

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

(org-srs-property-defcustom org-srs-item-cloze-visibility nil "")

(defun org-fc-item-cloze-string-pad-width (string width)
  (concat string (make-string (- width (string-width string)) ? )))

(defun org-fc-item-cloze-pom-at-table-p (point-or-marker)
  (save-excursion
    (goto-char point-or-marker)
    (org-at-table-p)))

(defun org-srs-item-cloze-overlay-text (overlay)
  (overlay-get overlay 'display))

(defun \(setf\ org-srs-item-cloze-overlay-text\) (text overlay)
  (overlay-put
   overlay 'display
   (if (org-fc-item-cloze-pom-at-table-p (overlay-start overlay))
       (org-fc-item-cloze-string-pad-width
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

(cl-defmethod org-srs-item-review ((type (eql 'cloze)) &rest args)
  (ignore type)
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
            (cl-destructuring-bind (overlay id start end text &optional hint) current
              (ignore id start end hint)
              (cl-assert (overlayp overlay))
              (lambda ()
                (setf (org-srs-item-cloze-overlay-text overlay) (org-srs-item-cloze-answer text)))))
           (org-srs-item-add-hook-once 'org-srs-review-after-rate-hook #'org-srs-item-cloze-remove-overlays)
           (apply (org-srs-item-confirmation) type args)))

(defun org-srs-item-cloze-sha1sum-short (content)
  (substring (sha1 content) 0 7))

(org-srs-property-defcustom org-srs-item-cloze-identifier #'org-srs-item-cloze-sha1sum-short "")

(defun org-srs-item-cloze-default (start end &optional hint)
  (let ((identifier (funcall (org-srs-item-cloze-identifier) (buffer-substring-no-properties start end))))
    (save-excursion
      (goto-char end)
      (if hint (insert "}{" hint "}}") (insert "}}"))
      (goto-char start)
      (insert "{{" identifier "}{"))))

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
   (ignore type props)
   (cl-destructuring-bind (start . end) (bounds-of-thing-at-point 'word)
     (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element start end (list type props)))))))

(defun org-srs-item-cloze-dwim ()
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
             do (replace-match (match-string 2)))))

(defvar org-srs-item-uncloze-function #'org-srs-item-uncloze-default)

(cl-defgeneric org-srs-item-uncloze (type &optional props)
  (:method
   ((type (eql 'paragraph)) &optional props)
   (let* ((element (list type props))
          (start (org-element-begin element))
          (end (org-element-end element)))
     (funcall org-srs-item-uncloze-function start end))))

(cl-defgeneric org-srs-item-uncloze-interactively (type &optional props)
  (:method
   (type &optional props)
   (org-srs-item-uncloze type props))
  (:method
   (type &context ((region-active-p) (eql t)) &optional props)
   (ignore type props)
   (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element (region-beginning) (region-end) (list type props)))))
  (:method
   (type &context ((region-active-p) (eql nil)) ((org-srs-item-cloze-bounds) cons) &optional props)
   (ignore type props)
   (cl-destructuring-bind (start . end) (org-srs-item-cloze-bounds)
     (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element start end (list type props)))))))

(defun org-srs-item-uncloze-dwim ()
  (interactive)
  (apply #'org-srs-item-uncloze-interactively (org-element-at-point)))

(defun org-srs-item-cloze-update-entry (&optional inherit-history-p)
  (let* ((start (org-entry-beginning-position))
         (end (org-entry-end-position))
         (items (cl-delete 'cloze (cl-mapcar #'cl-first (org-srs-query-region (org-srs-query-predicate-and) start end)) :key #'car :test-not #'eq))
         (clozes (org-srs-item-cloze-collect start end)))
    (setf inherit-history-p (if (= (length items) (length clozes))
                                (cl-etypecase inherit-history-p
                                  (boolean inherit-history-p)
                                  (function (funcall inherit-history-p)))
                              (cl-assert (not (eq inherit-history-p t)))))
    (if inherit-history-p
        (cl-loop for item in items
                 for (identifier) in clozes
                 do
                 (org-srs-item-goto item)
                 (beginning-of-line)
                 (cl-assert (looking-at org-srs-item-header-regexp))
                 (replace-match (cl-reduce (lambda (acc it) (format "%s::%s" acc it)) (list (car item) identifier)) nil nil nil 2))
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

(defun org-srs-item-cloze-update (arg)
  (interactive "p")
  (save-excursion
    (if (<= arg 1)
        (org-srs-item-cloze-update-entry
         (when (called-interactively-p 'interactive)
           (apply-partially #'y-or-n-p "Sequentially inherit review history from before the cloze modification?")))
      (cl-destructuring-bind (start . end) (org-srs-item-cloze-bounds)
        (goto-char start)
        (cl-assert (looking-at org-srs-item-cloze-regexp))
        (replace-match (save-match-data (funcall (org-srs-item-cloze-identifier) (match-string 2))) nil nil nil 1)
        (org-srs-item-cloze-update-entry (cl-constantly t))))
    (org-srs-log-hide-drawer)))

(org-srs-property-defcustom org-srs-item-cloze-table-range "@<<$<<..@>$>" "")

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

(cl-defmethod org-srs-item-new-interactively ((type (eql 'cloze)) &rest args)
  (ignore type args)
  (if args (cl-call-next-method) (org-srs-item-cloze-dwim)))

(provide 'org-srs-item-cloze)
