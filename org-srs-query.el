;;; org-srs-query.el --- Review item query facilities -*- lexical-binding:t -*-

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

;; This package provides operations for querying and filtering Org-srs
;; review items based on various composable predicates.

;;; Code:

(require 'cl-lib)
(require 'parse-time)
(require 'custom)

(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-item)
(require 'org-srs-time)

(defgroup org-srs-query nil
  "Searching and querying items matching specific predicates."
  :group 'org-srs
  :prefix "org-srs-query-")

(cl-defgeneric org-srs-query-predicate (object &rest args)
  (:method ((list list) &rest args) (cl-assert (null args)) (apply #'org-srs-query-predicate list))
  (:method ((name symbol) &rest args) (apply (intern (format "%s-%s" 'org-srs-query-predicate name)) args))
  (:method ((predicate function) &rest args) (if args (lambda () (apply predicate args)) predicate)))

(defun org-srs-query-item-p (predicate &rest item)
  (let ((predicate (org-srs-query-predicate predicate)))
    (if item (org-srs-item-with-current item (funcall predicate)) (funcall predicate))))

(defun org-srs-query-predicate-and (&rest predicates)
  (lambda () (cl-loop for predicate in predicates always (funcall predicate))))

(cl-defmethod org-srs-query-predicate ((_name (eql 'and)) &rest args)
  (apply #'org-srs-query-predicate-and (mapcar #'org-srs-query-predicate args)))

(defun org-srs-query-predicate-or (&rest predicates)
  (lambda () (cl-loop for predicate in predicates thereis (funcall predicate))))

(cl-defmethod org-srs-query-predicate ((_name (eql 'or)) &rest args)
  (apply #'org-srs-query-predicate-or (mapcar #'org-srs-query-predicate args)))

(defun org-srs-query-predicate-not (predicate)
  (lambda () (not (funcall predicate))))

(cl-defmethod org-srs-query-predicate ((_name (eql 'not)) &rest args)
  (cl-destructuring-bind (predicate) args
    (org-srs-query-predicate-not (org-srs-query-predicate predicate))))

(defun org-srs-query-predicate-new ()
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-srs-table-end))
        (forward-line -2)
        (org-at-table-hline-p)))))

(cl-defun org-srs-query-predicate-updated (&optional
                                           (from (org-srs-time-today) fromp)
                                           (to (unless fromp (org-srs-time-tomorrow))))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-srs-table-end))
        (forward-line -1)
        (unless (org-at-table-hline-p)
          (let ((time (org-srs-timestamp-time (org-srs-table-field 'timestamp))))
            (and (time-less-p from time) (or (null to) (time-less-p time to)))))))))

(cl-defun org-srs-query-predicate-due (&optional (now (org-srs-time-now)))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-srs-table-end))
        (time-less-p (org-srs-timestamp-time (match-string-no-properties 2)) now)))))

(cl-defun org-srs-query-predicate-learned (&optional
                                           (from (org-srs-time-today) fromp)
                                           (to (unless fromp (org-srs-time-tomorrow))))
  (lambda ()
    (save-excursion
      (goto-char (org-srs-table-begin))
      (when (org-table-goto-line 4)
        (beginning-of-line)
        (when (save-excursion (re-search-forward org-srs-log-latest-timestamp-regexp (org-srs-table-end) t))
          (forward-line -1)
          (when-let ((timestamp (org-srs-table-field 'timestamp)))
            (let ((time (org-srs-timestamp-time timestamp)))
              (and (time-less-p from time) (or (null to) (time-less-p time to))))))))))

(defun org-srs-query-predicate-reviewed (&rest args)
  (org-srs-query-predicate-and
   (org-srs-query-predicate-not (org-srs-query-predicate-new))
   (apply #'org-srs-query-predicate-updated args)))

(defun org-srs-query-predicate-suspended ()
  #'org-in-commented-heading-p)

(gv-define-expander cl-values
  (lambda (do &rest args)
    (funcall
     do `(cl-values . ,args)
     (lambda (values)
       (let ((vars (mapcar #'gensym args)))
         `(cl-destructuring-bind ,vars ,values
            (setf . ,(cl-mapcan #'list args vars))))))))

(defmacro org-srs-query-cl-loop (&rest args)
  (declare (indent 0))
  (let ((cl-loop (list 'cl-loop)) (end nil))
    (cl-symbol-macrolet ((start (cdr cl-loop)))
      (cl-with-gensyms (default-var default-cons)
        (append
         cl-loop
         (cl-loop with cons
                  for (collect val into var) = args
                  for has-finally = (or has-finally (eq collect 'finally))
                  if (eq collect 'collect)
                  do (setf (cl-values var cons end args)
                           (cl-case into
                             (into (cl-values var (intern (format "%s--%s" var 'cons)) end (cddddr args)))
                             (t (cl-values
                                 default-var default-cons
                                 (let ((clause `(cl-return ,default-var)))
                                   (if (cl-find clause end :test #'equal) end (append end (list clause))))
                                 (cddr args))))
                           start (let ((clause `(with ,var = nil and ,cons = nil)))
                                   (if (cl-search clause start) start (append start clause))))
                  and append `(do (setf ,cons ,(cl-once-only (val) `(if ,cons (setf (cdr ,cons) (list ,val)) (setf ,var (list ,val))))))
                  else
                  do (setf args (cdr args))
                  and collect collect
                  unless args unless has-finally
                  do (setf args '(finally (progn)) has-finally t)
                  while args)
         end)))))

(defmacro org-srs-query-with-loop (&rest body)
  (declare (indent 0))
  `(progn . ,(cl-subst 'org-srs-query-cl-loop 'cl-loop body)))

(cl-defun org-srs-query-region (predicate &optional (start (point-min)) (end (point-max)))
  (save-excursion
    (org-srs-query-with-loop
      (cl-loop initially (goto-char start)
               while (re-search-forward org-srs-item-header-regexp end t)
               do (goto-char (match-beginning 0))
               when (prog1 (save-match-data (funcall predicate)) (goto-char (match-end 0)))
               collect (cl-multiple-value-list (org-srs-item-from-match-data))))))

(cl-defun org-srs-query-buffer (predicate &optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (cl-loop with items = (org-srs-query-region predicate)
             with list = (list buffer)
             for item in items
             do (setf (nthcdr 2 item) list)
             finally (cl-return items))))

(cl-defun org-srs-query-file (predicate &optional (file (buffer-file-name (current-buffer))))
  (org-srs-query-buffer predicate (find-file-noselect file)))

(org-srs-property-defcustom org-srs-query-directory-file-regexp
  (rx bos (not ".") (*? anychar) ".org" (? ".gpg") eos)
  "File name regexp expression used to filter files to review in a directory."
  :group 'org-srs-query
  :type 'regexp)

(cl-defun org-srs-query-directory (predicate &optional (directory default-directory))
  (cl-loop for file in (directory-files-recursively directory (org-srs-query-directory-file-regexp))
           nconc (org-srs-query-file predicate file)))

(defun org-srs-query-rcurry (fun &rest arguments)
  (lambda (&rest args)
    (apply fun (nconc args arguments))))

(cl-defgeneric org-srs-query-function (source)
  (:method
   ((source cons))
   (org-srs-query-rcurry #'org-srs-query-region (car source) (cdr source)))
  (:method
   ((source buffer))
   (org-srs-query-rcurry #'org-srs-query-buffer source))
  (:method
   ((source string))
   (cl-assert (file-exists-p source))
   (if (file-directory-p source)
       (org-srs-query-rcurry #'org-srs-query-directory source)
     (org-srs-query-rcurry #'org-srs-query-file source))))

(cl-defun org-srs-query (predicate &optional (source (or (buffer-file-name) default-directory)))
  (funcall (org-srs-query-function source) (org-srs-query-predicate predicate)))

(provide 'org-srs-query)
;;; org-srs-query.el ends here
