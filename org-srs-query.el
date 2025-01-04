;;; org-srs-query.el --- Review item query facilities -*- lexical-binding:t -*-

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

;; This package provides operations for querying and filtering Org-srs
;; review items based on various composable predicates.

;;; Code:

(require 'cl-lib)
(require 'parse-time)

(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-item)
(require 'org-srs-time)

(cl-defgeneric org-srs-query-ensure-predicate (object &rest args)
  (:method ((name symbol) &rest args) (apply (intern (format "%s-%s" 'org-srs-query-predicate name)) args))
  (:method ((fun function) &rest args) (if args (lambda () (apply fun args)) fun)))

(defun org-srs-query-predicate (desc)
  (cl-typecase desc
    (function desc)
    (t (apply #'org-srs-query-ensure-predicate (ensure-list desc)))))

(defun org-srs-query-predicate-and (&rest predicates)
  (lambda () (cl-loop for predicate in predicates always (funcall predicate))))

(cl-defmethod org-srs-query-ensure-predicate ((_name (eql 'and)) &rest args)
  (apply #'org-srs-query-predicate-and (mapcar #'org-srs-query-predicate args)))

(defun org-srs-query-predicate-or (&rest predicates)
  (lambda () (cl-loop for predicate in predicates thereis (funcall predicate))))

(cl-defmethod org-srs-query-ensure-predicate ((_name (eql 'or)) &rest args)
  (apply #'org-srs-query-predicate-or (mapcar #'org-srs-query-predicate args)))

(defun org-srs-query-predicate-not (predicate)
  (lambda () (not (funcall predicate))))

(cl-defmethod org-srs-query-ensure-predicate ((_name (eql 'not)) &rest args)
  (cl-destructuring-bind (predicate) args
    (org-srs-query-predicate-not (org-srs-query-predicate predicate))))

(defun org-srs-query-predicate-new ()
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (forward-line -1)
        (or (org-at-table-hline-p) (string-empty-p (org-srs-table-field 'rating)))))))

(cl-defun org-srs-query-predicate-updated
    (&optional
     (from (org-srs-time-today) fromp)
     (to (unless fromp (org-srs-time-tomorrow))))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (forward-line -1)
        (unless (org-at-table-hline-p)
          (let ((time (org-srs-timestamp-time (org-srs-table-field 'timestamp))))
            (and (time-less-p from time) (or (null to) (time-less-p time to)))))))))

(cl-defun org-srs-query-predicate-due (&optional (now (org-srs-time-now)))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (time-less-p (org-srs-timestamp-time (match-string 2)) now)))))

(cl-defun org-srs-query-predicate-learned
    (&optional
     (from (org-srs-time-today) fromp)
     (to (unless fromp (org-srs-time-tomorrow))))
  (lambda ()
    (save-excursion
      (when-let ((time (cl-loop with end = (org-table-end)
                                initially (org-table-goto-line 2)
                                for previous-rating = "" then current-rating
                                for current-rating = (org-srs-table-field 'rating)
                                for current-timestamp = (org-srs-table-field 'timestamp)
                                when (and (string-empty-p previous-rating) (not (string-empty-p current-rating)))
                                return (org-srs-timestamp-time current-timestamp)
                                do (forward-line 1)
                                until (>= (point) end))))
        (and (time-less-p from time) (or (null to) (time-less-p time to)))))))

(defun org-srs-query-predicate-reviewed (&rest args)
  (org-srs-query-predicate-and
   (org-srs-query-predicate-not (org-srs-query-predicate-new))
   (apply #'org-srs-query-predicate-updated args)))

(cl-defun org-srs-query-region (predicate &optional (start (point-min)) (end (point-max)))
  (save-excursion
    (cl-loop initially (goto-char start)
             while (re-search-forward org-srs-item-header-regexp end t)
             when (save-match-data
                    (re-search-forward org-table-line-regexp)
                    (funcall predicate))
             collect (cl-multiple-value-list (org-srs-item-from-match-data)))))

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
  (rx bos (not ".") (+? anychar) ".org" (? ".gpg") eos)
  "The file name regexp used to filter files to review in a directory."
  :group 'org-srs
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
