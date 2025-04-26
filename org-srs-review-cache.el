;;; org-srs-review-cache.el --- Cache query results in a review session -*- lexical-binding:t -*-

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

;; This package provides caching mechanisms for query results in a
;; review session, speeding up retrieving the next review item.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'pcase)
(require 'custom)

(require 'org)
(require 'org-element)

(require 'org-srs-time)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-property)
(require 'org-srs-query)

(defgroup org-srs-review-cache nil
  "Caching mechanisms to improve scheduling performance in a review session."
  :group 'org-srs-review
  :prefix "org-srs-review-cache-")

(cl-defstruct (org-srs-review-cache (:predicate org-srs-review-cache--p))
  (source nil :type t)
  (time (floor (time-to-seconds)) :type fixnum)
  (queries nil :type list)
  (pending nil :type list)
  (due-times (make-hash-table :test #'equal) :type hash-table)
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

(cl-defun org-srs-review-cache-update-pending-1 (&optional (cache (org-srs-review-cache)))
  (setf (org-srs-review-cache-pending cache)
        (cl-loop with queries = (org-srs-review-cache-queries cache)
                 for time-predicate-item in (org-srs-review-cache-pending cache)
                 for (time . (predicate . item)) = time-predicate-item
                 for due-time = (org-srs-review-cache-query-predicate-due-time predicate)
                 if (org-srs-time> time due-time)
                 collect time-predicate-item
                 else
                 do (cl-assert (cl-assoc predicate queries :test #'equal))
                 (push item (alist-get predicate queries nil nil #'equal)))))

(cl-defun org-srs-review-cache-update-pending (&optional (cache (org-srs-review-cache)))
  (let ((time (floor (time-to-seconds (org-srs-time-now)))))
    (cl-assert (>= time (org-srs-review-cache-time cache)))
    (when (> time (org-srs-review-cache-time cache))
      (org-srs-review-cache-update-pending-1 cache)
      (setf (org-srs-review-cache-time cache) time))))

(defun org-srs-review-cache-query (predicate &optional source)
  (if-let ((cache (org-srs-review-cache)))
      (let ((queries (org-srs-review-cache-queries cache)))
        (cl-assert (org-srs-review-cache-source cache))
        (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
        (org-srs-review-cache-update-pending cache)
        (alist-get predicate queries org-srs-review-cache-null t #'equal))
    org-srs-review-cache-null))

(defun \(setf\ org-srs-review-cache-query\) (value predicate &optional source)
  (let ((cache (or (org-srs-review-cache) (setf (org-srs-review-cache) (make-org-srs-review-cache :source source)))))
    (cl-assert (org-srs-review-cache-source cache))
    (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
    (setf (alist-get predicate (org-srs-review-cache-queries cache) org-srs-review-cache-null t #'equal) value)))

(org-srs-property-defcustom org-srs-review-cache-p nil
  "Non-nil means to enable caching mechanisms in a review session.

This can increase the speed of retrieving the next review item
from a large set of review items."
  :group 'org-srs-review-cache
  :type 'boolean)

(define-advice org-srs-review-cache-p (:around (fun &rest args) org-srs-review-cache)
  (if (and args (null (cdr args))) (apply #'org-srs-review-cache--p args) (apply fun args)))

(defun org-srs-review-cache-active-p ()
  (and (org-srs-reviewing-p) (org-srs-review-cache-p)))

(defmacro org-srs-review-cache-ensure-gethash (key hash-table &optional default)
  (cl-once-only (key hash-table)
    (cl-with-gensyms (value null)
      `(let ((,value (gethash ,key ,hash-table ',null)))
         (if (eq ,value ',null)
             (setf (gethash ,key ,hash-table ',null) ,default)
           ,value)))))

(defun org-srs-review-cache-item (&rest args)
  (cl-destructuring-bind (item &optional (id (org-id-get)) (buffer (current-buffer))) args
    (list item id buffer)))

(define-advice org-srs-item-due-time (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) args)
      (let ((args (apply #'org-srs-review-cache-item args))
            (due-times (org-srs-review-cache-due-times (org-srs-review-cache))))
        (org-srs-review-cache-ensure-gethash args due-times (apply fun args)))
    (apply fun args)))

(define-advice org-srs-item-marker (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) args)
      (let ((args (apply #'org-srs-review-cache-item args))
            (markers (org-srs-review-cache-markers (org-srs-review-cache))))
        (org-srs-review-cache-ensure-gethash args markers (apply fun args)))
    (apply fun args)))

(define-advice org-srs-item-goto (:around (fun &rest args) org-srs-review-cache)
  (cl-assert args)
  (defvar org-srs-item-marker@org-srs-review-cache)
  (if (and (not (bound-and-true-p org-srs-item-marker@org-srs-review-cache)) (org-srs-review-cache-active-p))
      (let ((marker (let ((org-srs-item-marker@org-srs-review-cache t)) (apply #'org-srs-item-marker args))))
        (switch-to-buffer (marker-buffer marker) nil t)
        (goto-char marker))
    (apply fun args)))

(defun org-srs-review-cache-item-equal (item-a item-b)
  (cl-assert item-a) (cl-assert item-b)
  ;; (cl-every #'equal item-a item-b)
  (cl-loop for elem-a in item-a for elem-b in item-b always (equal elem-a elem-b)))

(define-advice org-srs-query (:around (fun &rest args) org-srs-review-cache)
  (cl-destructuring-bind (predicate &optional (source (or (buffer-file-name) default-directory))) args
    (if (org-srs-review-cache-active-p)
        (let ((result (org-srs-review-cache-query predicate source)))
          (if (eq result org-srs-review-cache-null)
              (let ((query-predicate
                     (if-let ((due-time (org-srs-review-cache-query-predicate-due-time predicate)))
                         (pcase-exhaustive predicate
                           (`(and ,(and (or 'due `(due . ,_)) first-predicate) . ,rest-predicates)
                            (let ((tomorrow-time (org-srs-time-tomorrow))
                                  (cache (org-srs-review-cache)))
                              (cl-flet* ((cache-item ()
                                           (nconc (cl-multiple-value-list (org-srs-item-at-point)) (list (current-buffer))))
                                         (cache-marker (&optional (item (cache-item)))
                                           (setf (gethash item (org-srs-review-cache-markers cache)) (point-marker)))
                                         (cache-due-time ()
                                           (let ((due-time (org-srs-item-due-time)))
                                             (when (org-srs-time> tomorrow-time due-time)
                                               (let ((item (cache-item)))
                                                 (setf (org-srs-review-cache-pending cache)
                                                       (cons (cons due-time (cons predicate item))
                                                             (cl-delete (cons predicate item) (org-srs-review-cache-pending cache)
                                                                        :key #'cdr :test #'org-srs-review-cache-item-equal)))
                                                 (cache-marker item))))
                                           nil))
                                `(and ,@rest-predicates (or (and ,first-predicate ,#'cache-marker) ,#'cache-due-time))))))
                       predicate)))
                (setf (org-srs-review-cache-query predicate source) (funcall fun query-predicate source)))
            result))
      (funcall fun predicate source))))

(define-advice org-srs-review-start (:around (fun &rest args) org-srs-review-cache)
  (org-srs-property-let (org-srs-review-cache-p)
    (apply fun args)))

(define-advice org-srs-review-rate (:around (fun &rest args) org-srs-review-cache)
  (org-srs-property-let (org-srs-review-cache-p)
    (apply fun args)))

(defun org-srs-review-cache-after-rate ()
  (defvar org-srs-review-rating)
  (when (org-srs-review-cache-p)
    (if org-srs-review-rating
        (org-srs-item-with-current org-srs-review-item
          (let* ((cache (org-srs-review-cache))
                 (item (cl-multiple-value-call #'org-srs-review-cache-item (org-srs-item-at-point)))
                 (due-time (setf (gethash item (org-srs-review-cache-due-times cache)) (org-srs-item-due-time))))
            (setf (org-srs-review-cache-pending cache)
                  (cl-delete item (org-srs-review-cache-pending cache) :key #'cddr :test #'org-srs-review-cache-item-equal)
                  (org-srs-review-cache-queries cache)
                  (cl-loop with tomorrow-time = (org-srs-time-tomorrow)
                           for (predicate . items) in (org-srs-review-cache-queries cache)
                           for (all-satisfied-p . rest-satisfied-p)
                           = (pcase predicate
                               (`(and ,(and (or 'due `(due . ,_)) first-predicate) . ,rest-predicates)
                                (when (funcall (org-srs-query-predicate `(and . ,rest-predicates)))
                                  (cons (funcall (org-srs-query-predicate first-predicate)) t)))
                               (_ (cons (funcall (org-srs-query-predicate predicate)) nil)))
                           collect (cons predicate (funcall (if all-satisfied-p #'cl-adjoin #'cl-delete)
                                                            item items :test #'org-srs-review-cache-item-equal))
                           when (org-srs-time> tomorrow-time due-time)
                           when (and rest-satisfied-p (not all-satisfied-p))
                           do (push (cons due-time (cons predicate item)) (org-srs-review-cache-pending cache))))))
      (org-srs-review-cache-clear))))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-review-cache-after-rate 95)

(defun org-srs-review-cache-sub-predicate-p (pred1 pred2)
  (pcase pred2
    ((guard (equal pred1 pred2)) 'equal)
    (`(and . ,preds) (cl-loop for pred2 in preds thereis (org-srs-review-cache-sub-predicate-p pred1 pred2)))))

(define-advice org-srs-query-predicate (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) (not (bound-and-true-p org-srs-query-predicate@org-srs-review-cache)))
      (cl-destructuring-bind (desc) args
        (cl-etypecase desc
          ((or list symbol)
           (lambda (&rest args)
             (cl-loop with cache = (org-srs-review-cache)
                      with target-item = (or args (cl-multiple-value-list (org-srs-item-at-point)))
                      initially (cl-assert (org-srs-review-cache-active-p))
                      for (predicate . items) in (org-srs-review-cache-queries cache)
                      for sub-predicate-p = (org-srs-review-cache-sub-predicate-p desc predicate)
                      when sub-predicate-p
                      if (cl-loop for item in items thereis (org-srs-review-cache-item-equal item target-item)) return t
                      else if (eq sub-predicate-p 'equal) return nil
                      finally (cl-return (cl-member target-item (org-srs-query desc (org-srs-review-cache-source cache))
                                                    :test #'org-srs-review-cache-item-equal)))))
          (function desc)))
    (apply fun args)))

(defmacro org-srs-review-cache-without-query-predicate (&rest body)
  (declare (indent 0))
  `(cl-locally
    (defvar org-srs-query-predicate@org-srs-review-cache)
    (let ((org-srs-query-predicate@org-srs-review-cache t)) . ,body)))

(define-advice org-srs-review-cache-after-rate (:around (fun &rest args) org-srs-query-predicate@org-srs-review-cache)
  (org-srs-review-cache-without-query-predicate (apply fun args)))

(define-advice org-srs-query (:around (fun &rest args) org-srs-query-predicate@org-srs-review-cache)
  (org-srs-review-cache-without-query-predicate (apply fun args)))

(define-advice org-srs-query-item-p (:around (fun . (predicate &rest item)) org-srs-query-predicate@org-srs-review-cache)
  (if (org-srs-review-cache-active-p)
      (progn
        (cl-assert (not (bound-and-true-p org-srs-query-predicate@org-srs-review-cache)))
        (if item (apply predicate item) (apply fun predicate item)))
    (apply fun predicate item)))

(provide 'org-srs-review-cache)
;;; org-srs-review-cache.el ends here
