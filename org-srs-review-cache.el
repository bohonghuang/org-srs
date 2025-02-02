;;; org-srs-review-cache.el --- Cache query results in a review session -*- lexical-binding:t -*-

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

;; This package provides a caching mechanism for query results in a
;; review session, speeding up retrieving the next review item.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'pcase)

(require 'org)
(require 'org-element)

(require 'org-srs-time)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-property)
(require 'org-srs-query)

(cl-defstruct org-srs-review-cache
  (source nil :type t)
  (queries nil :type list)
  (pending nil :type list)
  (markers (make-hash-table :test #'equal) :type hash-table))

(defvar org-srs-review-cache nil)
(defsubst org-srs-review-cache () org-srs-review-cache)
(defsubst \(setf\ org-srs-review-cache\) (value) (setf org-srs-review-cache value))

(cl-pushnew #'org-srs-review-cache org-srs-reviewing-predicates)

(defun org-srs-review-cache-clear (&rest _args)
  (setf (org-srs-review-cache) nil))

(add-hook 'org-srs-review-finish-hook #'org-srs-review-cache-clear)

(cl-defun org-srs-review-cache-query-predicate-due-time (predicate)
  (pcase predicate
    (`(and ,(or 'due `(due . ,args)) . ,_)
     (cl-destructuring-bind (&optional (time (org-srs-time-now))) args time))))

(defconst org-srs-review-cache-null (make-symbol (symbol-name 'nil)))

(defun org-srs-review-cache-query (predicate &optional source)
  (if-let ((cache (org-srs-review-cache)))
      (cl-symbol-macrolet ((queries (org-srs-review-cache-queries cache)))
        (cl-assert (org-srs-review-cache-source cache))
        (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
        (save-window-excursion
          (save-excursion
            (setf (org-srs-review-cache-pending cache)
                  (cl-loop for time-predicate-item in (org-srs-review-cache-pending cache)
                           for (time . (predicate . item)) = time-predicate-item
                           for due-time = (org-srs-review-cache-query-predicate-due-time predicate)
                           if (cl-plusp (org-srs-time-difference time due-time))
                           collect time-predicate-item
                           else
                           do (push item (alist-get predicate queries nil nil #'equal))))
            (alist-get predicate queries org-srs-review-cache-null t #'equal))))
    org-srs-review-cache-null))

(defun \(setf\ org-srs-review-cache-query\) (value predicate &optional source)
  (let ((cache (or (org-srs-review-cache) (setf (org-srs-review-cache) (make-org-srs-review-cache :source source)))))
    (cl-assert (org-srs-review-cache-source cache))
    (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
    (setf (alist-get predicate (org-srs-review-cache-queries cache) org-srs-review-cache-null t #'equal) value)))

(org-srs-property-defcustom org-srs-review-cache-query-p nil
  "Non-nil means to cache query results in a review session.

This can increase the speed of retrieving the next review item
from a large set of review items."
  :group 'org-srs
  :type 'boolean)

(define-advice org-srs-item-goto (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-reviewing-p) (org-srs-review-cache-query-p))
      (cl-destructuring-bind (item &optional (id (org-id-get)) (buffer (current-buffer)) &aux (args (list item id buffer))) args
        (let* ((markers (org-srs-review-cache-markers (org-srs-review-cache)))
               (marker (or (gethash args markers) (setf (gethash args markers) (progn (apply fun args) (point-marker))))))
          (switch-to-buffer (marker-buffer marker) nil t)
          (goto-char marker)))
    (apply fun args)))

(define-advice org-srs-query (:around (fun &rest args) org-srs-review-cache)
  (cl-destructuring-bind (predicate &optional (source (or (buffer-file-name) default-directory))) args
    (if (and (org-srs-reviewing-p) (org-srs-review-cache-query-p))
        (let ((result (org-srs-review-cache-query predicate source)))
          (if (eq result org-srs-review-cache-null)
              (let ((query-predicate
                     (if-let ((due-time (org-srs-review-cache-query-predicate-due-time predicate)))
                         (pcase-exhaustive predicate
                           (`(and ,due-predicate . ,rest-predicates)
                            (let* ((tomorrow-time (org-srs-time-tomorrow))
                                   (cache (org-srs-review-cache))
                                   (cache-predicate
                                    (lambda ()
                                      (let ((due-time (org-srs-timestamp-time (org-srs-item-due-timestamp))))
                                        (when (cl-plusp (org-srs-time-difference tomorrow-time due-time))
                                          (let ((item (nconc (cl-multiple-value-list (org-srs-item-at-point)) (list (current-buffer)))))
                                            (setf (org-srs-review-cache-pending cache)
                                                  (cons (cons due-time (cons predicate item))
                                                        (cl-delete (cons predicate item) (org-srs-review-cache-pending cache) :key #'cdr :test #'equal))))))
                                      nil)))
                              `(and ,@rest-predicates (or ,due-predicate ,cache-predicate)))))
                       predicate)))
                (setf (org-srs-review-cache-query predicate source) (funcall fun query-predicate source)))
            result))
      (funcall fun predicate source))))

(defun org-srs-review-cache-after-rate ()
  (defvar org-srs-review-rating)
  (if (and org-srs-review-rating (org-srs-review-cache-query-p))
      (with-current-buffer (marker-buffer org-srs-review-item-marker)
        (save-excursion
          (goto-char org-srs-review-item-marker)
          (goto-char (org-table-begin))
          (save-restriction
            (let* ((cache (org-srs-review-cache))
                   (element (org-element-at-point))
                   (due-time (org-srs-timestamp-time (org-srs-item-due-timestamp))))
              (narrow-to-region (org-element-begin element) (1+ (org-element-end element)))
              (goto-char (org-table-begin))
              (let ((item (nconc (cl-multiple-value-list (org-srs-item-at-point)) (list (current-buffer)))))
                (setf (org-srs-review-cache-pending cache)
                      (cl-delete item (org-srs-review-cache-pending cache) :key #'cddr :test #'equal)
                      (org-srs-review-cache-queries cache)
                      (cl-loop with tomorrow-time = (org-srs-time-tomorrow)
                               for (predicate . items) in (org-srs-review-cache-queries cache)
                               for all-satisfied-p = (funcall (org-srs-query-predicate predicate))
                               for rest-satisfied-p = (pcase predicate
                                                        (`(and ,(or 'due `(due . ,_)) . ,rest-predicates)
                                                         (funcall (org-srs-query-predicate `(and . ,rest-predicates)))))
                               collect (cons predicate (funcall (if all-satisfied-p #'cl-adjoin #'cl-delete)
                                                                item items :test #'equal))
                               when (cl-plusp (org-srs-time-difference tomorrow-time due-time))
                               when (and rest-satisfied-p (not all-satisfied-p))
                               do (push (cons due-time (cons predicate item)) (org-srs-review-cache-pending cache)))))))))
    (org-srs-review-cache-clear)))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-review-cache-after-rate 95)

(provide 'org-srs-review-cache)
;;; org-srs-review-cache.el ends here
