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

(require 'org)
(require 'org-element)

(require 'org-srs-time)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-property)
(require 'org-srs-query)

(cl-defstruct org-srs-review-cache
  (source nil)
  (queries nil)
  (undue nil))

(defvar org-srs-review-cache nil)
(defsubst org-srs-review-cache () org-srs-review-cache)
(defsubst \(setf\ org-srs-review-cache\) (value) (setf org-srs-review-cache value))

(cl-pushnew #'org-srs-review-cache org-srs-reviewing-predicates)

(defun org-srs-review-cache-invalidate (&rest _args)
  (setf (org-srs-review-cache) nil))

(add-hook 'org-srs-review-finish-hook #'org-srs-review-cache-invalidate)

(defun org-srs-review-cache-item-equal (a b)
  (cl-loop for elem-a in a for elem-b in b repeat 2 always (equal elem-a elem-b)))

(defun org-srs-review-cache-query-predicate-due-time (predicate)
  (ignore-errors
    (cl-destructuring-bind (name &optional (time (org-srs-time-now))) (cadr predicate)
      (when (eq name 'due) time))))

(defconst org-srs-review-cache-null (make-symbol (symbol-name 'nil)))

(defun org-srs-review-cache-query (predicate &optional source)
  (if-let ((cache (org-srs-review-cache)))
      (cl-symbol-macrolet ((queries (org-srs-review-cache-queries cache)))
        (cl-assert (org-srs-review-cache-source cache))
        (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
        (save-window-excursion
          (save-excursion
            (setf (org-srs-review-cache-undue cache)
                  (cl-loop for time-predicate-item in (org-srs-review-cache-undue cache)
                           for (time . (predicate . item)) = time-predicate-item
                           for due-time = (org-srs-review-cache-query-predicate-due-time predicate)
                           if (cl-plusp (org-srs-time-difference time due-time))
                           collect time-predicate-item
                           else
                           do (push item (alist-get predicate queries nil nil #'equal))))
            (alist-get predicate queries org-srs-review-cache-null t #'equal))))
    org-srs-review-cache-null))

(defun org-srs-review-cache-due-time (cache)
  (car (cl-find-if #'org-srs-review-cache-query-predicate-due-time (org-srs-review-cache-queries cache) :key #'car)))

(defun \(setf\ org-srs-review-cache-query\) (value predicate &optional source)
  (let ((cache (or (org-srs-review-cache) (setf (org-srs-review-cache) (make-org-srs-review-cache :source source)))))
    (cl-assert (org-srs-review-cache-source cache))
    (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
    (setf (alist-get predicate (org-srs-review-cache-queries cache) org-srs-review-cache-null t #'equal) value)))

(org-srs-property-defcustom org-srs-review-cache-query-p nil
  "Non-nil means to cache query results in a review session.

This can increase the speed of retrieving the next review item
from a large set of review items, but it may reduce scheduling
accuracy."
  :group 'org-srs
  :type 'boolean)

(define-advice org-srs-query (:around (fun predicate &optional source) org-srs-review-query-with-cache)
  (if (and (org-srs-review-cache-query-p) (org-srs-reviewing-p))
      (let ((result (org-srs-review-cache-query predicate source)))
        (if (eq result org-srs-review-cache-null)
            (setf (org-srs-review-cache-query predicate source) (funcall fun predicate source))
          result))
    (funcall fun predicate source)))

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
                (setf (org-srs-review-cache-undue cache)
                      (cl-delete item (org-srs-review-cache-undue cache) :key #'cddr :test #'org-srs-review-cache-item-equal)
                      (org-srs-review-cache-queries cache)
                      (cl-loop for (predicate . items) in (org-srs-review-cache-queries cache)
                               for all-satisfied-p = (funcall (org-srs-query-predicate predicate))
                               for rest-satisfied-p = (and (listp predicate) (funcall (org-srs-query-predicate (cons (car predicate) (cddr predicate)))))
                               collect (cons predicate (funcall (if all-satisfied-p #'cl-adjoin #'cl-delete)
                                                                item items :test #'org-srs-review-cache-item-equal))
                               when (cl-plusp (org-srs-time-difference (org-srs-time-tomorrow) due-time))
                               when (and rest-satisfied-p (not all-satisfied-p))
                               do (push (cons due-time (cons predicate item)) (org-srs-review-cache-undue cache)))))))))
    (org-srs-review-cache-invalidate)))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-review-cache-after-rate 95)

(provide 'org-srs-review-cache)
;;; org-srs-review-cache.el ends here
