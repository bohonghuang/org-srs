;; -*- lexical-binding: t; -*-

(require 'rx)
(require 'org)
(require 'org-table)

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

(defconst org-srs-table-starred-line-regexp (rx bol (* blank) "|" (* blank) (group "*") (* blank) "|"))

(defun org-srs-table-goto-starred-line ()
  (prog2 (goto-char (org-table-begin))
      (re-search-forward org-srs-table-starred-line-regexp (org-table-end))
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
        (org-table-next-row))
       ((cl-minusp offset)
        (forward-line n)))
      (org-srs-table-current-line))))

(defun org-srs-table-forward-star ()
  (when (org-srs-table-goto-starred-line)
    (goto-char (match-beginning 1))
    (org-table-rotate-recalc-marks " ")
    (org-table-next-row)
    (org-table-rotate-recalc-marks "*")))

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
