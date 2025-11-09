;;; org-srs-review-cache.el --- Cache query results in a review session -*- lexical-binding: t -*-

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

;; This package provides caching mechanisms for query results in a
;; review session, speeding up retrieving the next review item.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'pcase)
(require 'custom)
(require 'avl-tree)

(require 'org)
(require 'org-element)

(require 'org-srs-time)
(require 'org-srs-entry)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-review-rate)
(require 'org-srs-property)
(require 'org-srs-query)

(defgroup org-srs-review-cache nil
  "Caching mechanisms to improve scheduling performance in a review session."
  :group 'org-srs-review
  :prefix "org-srs-review-cache-")

(cl-defstruct (org-srs-review-cache (:predicate org-srs-review-cache--p))
  "Cache structure for review session items.

SOURCE is the review session source.
TIME is the last update time of the cache.
QUERIES is an alist mapping queries to sets of review items.
PENDING is an alist mapping predicates to AVL trees of pending review items
sorted by due time.
DUE-TIMES is a hash table caching review items to their due times.
MARKERS is a hash table caching review items to their markers."
  (source nil :type t)
  (time (floor (time-to-seconds)) :type fixnum)
  (queries nil :type list)
  (pending nil :type list)
  (due-times (make-hash-table :test #'equal) :type hash-table)
  (markers (make-hash-table :test #'equal) :type hash-table))

(defvar org-srs-review-cache nil
  "Review cache used for the current review session.")

(defun org-srs-review-cache ()
  "Return the current review cache."
  org-srs-review-cache)

(cl-defmethod (setf org-srs-review-cache) (value)
  "Set the current review cache to VALUE."
  (setf org-srs-review-cache value))

(cl-pushnew #'org-srs-review-cache org-srs-reviewing-predicates)

(defun org-srs-review-cache-clear (&rest _args)
  "Clear the current review cache."
  (setf (org-srs-review-cache) nil))

(cl-defun org-srs-review-cache-avl-tree-delete-range (tree data &key (callback #'ignore) (direction '<=))
  "Delete nodes in TREE that satisfy the comparison with DATA in DIRECTION.
CALLBACK is called with the data of each deleted node."
  (let ((compare-function (avl-tree--cmpfun tree)) (root (avl-tree--dummyroot tree)))
    (cl-multiple-value-bind (compare-function node-left node-right dir-left dir-right)
        (cl-ecase direction
          (<= (cl-values compare-function #'avl-tree--node-left #'avl-tree--node-right 0 1))
          (>= (cl-values (lambda (a b) (funcall compare-function b a)) #'avl-tree--node-right #'avl-tree--node-left 1 0))
          (< (cl-values (lambda (a b) (not (funcall compare-function b a))) #'avl-tree--node-left #'avl-tree--node-right 0 1))
          (> (cl-values (lambda (a b) (not (funcall compare-function a b))) #'avl-tree--node-right #'avl-tree--node-left 1 0)))
      (cl-macrolet ((node-left (&rest args) `(funcall node-left . ,args))
                    (node-right (&rest args) `(funcall node-right . ,args))
                    (node-data (&rest args) `(avl-tree--node-data . ,args))
                    (node-branch (&rest args) `(avl-tree--node-branch . ,args)))
        (cl-labels ((traverse (node)
                      (when node
                        (traverse (node-left node))
                        (traverse (node-right node))
                        (funcall callback (node-data node))))
                    (recurse (node branch)
                      (cl-symbol-macrolet ((br (node-branch node branch)))
                        (cond
                         ((null br))
                         ((funcall compare-function data (node-data br))
                          (recurse br dir-left))
                         (t
                          (recurse br dir-right)
                          (traverse (node-left br))
                          (funcall callback (node-data br))
                          (setf br (node-right br)))))))
          (recurse root 0))))))

(defun org-srs-review-cache-pending< (a b)
  "Compare two pending review items A and B for ordering in pending queues."
  (let ((seconds-a (org-srs-time-seconds (car a)))
        (seconds-b (org-srs-time-seconds (car b))))
    (if (= seconds-a seconds-b)
        (< (sxhash-equal (cdr a)) (sxhash-equal (cdr b)))
      (< seconds-a seconds-b))))

(cl-defun org-srs-review-cache-pending-queue (predicate &optional (cache (org-srs-review-cache)))
  "Return the pending queue AVL tree for PREDICATE in CACHE."
  (or #1=(alist-get predicate (org-srs-review-cache-pending cache) nil nil #'equal)
      (setf #1# (avl-tree-create #'org-srs-review-cache-pending<))))

(defmacro org-srs-review-cache-ensure-gethash (key hash-table &optional default)
  "Get the value from HASH-TABLE for KEY, setting it to DEFAULT if not found."
  (cl-once-only (key hash-table)
    (cl-with-gensyms (value null)
      `(let ((,value (gethash ,key ,hash-table ',null)))
         (if (eq ,value ',null)
             (setf (gethash ,key ,hash-table ',null) ,default)
           ,value)))))

(defun org-srs-review-cache-query-predicate-due (predicate)
  "Extract the due predicate from PREDICATE if it contains a due clause.

Return the due predicate and the remaining predicate as multiple values."
  (pcase predicate
    ((or #1=(and (or 'due `(due . ,_)) due) `(and ,#1# . ,rest))
     (cl-values (ensure-list due) `(and . ,rest)))
    (_ (cl-values nil predicate))))

(defun org-srs-review-cache-query-predicate-due-time (predicate)
  "Extract the due time from PREDICATE if it contains a due clause."
  (pcase (cl-nth-value 0 (org-srs-review-cache-query-predicate-due predicate))
    (`(due . ,args) (cl-destructuring-bind (&optional (time (org-srs-time-now))) args time))))

(defun org-srs-review-cache-query-predicate-function ()
  "Return a closure that acts as `org-srs-query-predicate' but reuses some results."
  (let ((predicate-cache (make-hash-table :test #'equal)) (funcall-predicate-cache (make-hash-table :test #'equal)))
    (cl-labels ((funcall-predicate (predicate)
                  (pcase predicate
                    (`(and . ,predicates) (cl-loop for predicate in predicates always (funcall-predicate predicate)))
                    (`(or . ,predicates) (cl-loop for predicate in predicates thereis (funcall-predicate predicate)))
                    (`(not ,predicate) (not (funcall-predicate predicate)))
                    (predicate
                     (org-srs-review-cache-ensure-gethash
                      predicate funcall-predicate-cache
                      (funcall (org-srs-review-cache-ensure-gethash
                                predicate predicate-cache (org-srs-query-predicate predicate))))))))
      (lambda (&optional predicate)
        (if predicate (if (funcall-predicate predicate) #'always #'ignore) (clrhash funcall-predicate-cache))))))

(defmacro org-srs-review-cache-with-query-predicate-cache (&rest body)
  "Execute BODY with `org-srs-query-predicate' rebound to cache predicate results."
  (declare (indent 0))
  (cl-with-gensyms (org-srs-query-predicate args)
    `(let ((,org-srs-query-predicate (org-srs-review-cache-query-predicate-function)))
       (cl-flet ((org-srs-query-predicate (&rest ,args) (apply ,org-srs-query-predicate ,args)))
         ,@body))))

(cl-defun org-srs-review-cache-update-pending-1 (&optional (cache (org-srs-review-cache)))
  "Dequeue due pending review items and update cached query results in CACHE."
  (org-srs-review-cache-with-query-predicate-cache
    (cl-loop with queries = (org-srs-review-cache-queries cache)
             for (due-predicate . tree) in (org-srs-review-cache-pending cache)
             for due-time = (org-srs-review-cache-query-predicate-due-time due-predicate)
             do (org-srs-review-cache-avl-tree-delete-range
                 tree (cons due-time most-positive-fixnum)
                 :callback (lambda (due-time-item)
                             (let ((item (cdr due-time-item)))
                               (org-srs-item-with-current item
                                 (cl-loop initially (org-srs-query-predicate)
                                          for (predicate . items) in queries
                                          when (equal (cl-nth-value 0 (org-srs-review-cache-query-predicate-due predicate)) due-predicate)
                                          if (funcall (org-srs-query-predicate predicate)) do (setf (gethash item items) t)
                                          else do (cl-assert (not (gethash item items)))))))))))

(cl-defun org-srs-review-cache-update-pending (&optional (cache (org-srs-review-cache)))
  "Update pending review items and cached query results in CACHE if necessary."
  (let ((time (floor (time-to-seconds (org-srs-time-now)))))
    (cl-assert (>= time (org-srs-review-cache-time cache)))
    (when (> time (org-srs-review-cache-time cache))
      (org-srs-review-cache-update-pending-1 cache)
      (setf (org-srs-review-cache-time cache) time))))

(defconst org-srs-review-cache-null (make-symbol (symbol-name 'nil))
  "Special nil indicating a cache miss for query results.")

(defun org-srs-review-cache-query (predicate &optional source)
  "Query the cache for items matching PREDICATE in SOURCE."
  (if-let ((cache (org-srs-review-cache)))
      (let ((queries (org-srs-review-cache-queries cache)))
        (cl-assert (org-srs-review-cache-source cache))
        (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
        (org-srs-review-cache-update-pending cache)
        (if-let ((items (alist-get predicate queries nil t #'equal)))
            (hash-table-keys items)
          org-srs-review-cache-null))
    org-srs-review-cache-null))

(cl-defmethod (setf org-srs-review-cache-query) (value predicate &optional source)
  "Set the cached query result for PREDICATE in SOURCE to VALUE."
  (let ((cache (org-srs-review-cache)))
    (cl-assert (org-srs-review-cache-source cache))
    (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
    (setf (alist-get predicate (org-srs-review-cache-queries cache) nil t #'equal)
          (cl-loop with items = (make-hash-table :test #'equal)
                   for item-cons on value
                   do (setf (gethash (setf (car item-cons) (apply #'org-srs-review-cache-item (car item-cons))) items) t)
                   finally (cl-return items)))
    value))

(org-srs-property-defcustom org-srs-review-cache-p nil
  "Non-nil means to enable caching mechanisms in a review session.

This can increase the speed of retrieving the next review item
from a large set of review items."
  :group 'org-srs-review-cache
  :type 'boolean)

(define-advice org-srs-review-cache-p (:around (fun &rest args) org-srs-review-cache)
  (if (and args (null (cdr args)))
      (cl-typecase (car args)
        (function (apply fun args))
        (t (apply #'org-srs-review-cache--p args)))
    (apply fun args)))

(defun org-srs-review-cache-active-p ()
  "Return non-nil if review caching is active in the current review session."
  (and (org-srs-reviewing-p) (org-srs-review-cache-p)))

(defun org-srs-review-cache-item (&rest args)
  "Return the full specification for the review item specified by ARGS as a list."
  (cl-multiple-value-list (apply #'org-srs-item-full args)))

(defun org-srs-review-cache-nth (n list &optional default)
  "Return the Nth element of LIST, or DEFAULT if N is out of bounds."
  (cl-loop for i from 0
           for elem in list
           when (= i n)
           return elem
           finally (cl-return default)))

(gv-define-expander org-srs-review-cache-nth
  (lambda (do n list &rest args)
    (funcall
     do `(org-srs-review-cache-nth ,n ,list . ,args)
     (lambda (value)
       (cl-with-gensyms (dummy-cons cons block)
         `(cl-loop named ,block
                   with ,dummy-cons = (cons nil ,list)
                   for ,cons = ,dummy-cons then (cdr ,cons)
                   repeat (1+ ,n)
                   unless (cdr ,cons)
                   do (setf (cdr ,cons) (cons nil nil))
                   finally (cl-return-from ,block (setf ,list (cdr ,dummy-cons) (car ,cons) ,value))))))))

(defmacro org-srs-review-cache-ensure-nth (n list &optional default)
  "Get the value from LIST at index N, setting it to DEFAULT if not found."
  (cl-once-only (n)
    (cl-with-gensyms (value null)
      `(let ((,value (org-srs-review-cache-nth ,n ,list ',null)))
         (if (eq ,value ',null)
             (setf (org-srs-review-cache-nth ,n ,list ',null) ,default)
           ,value)))))

(define-advice org-srs-item-due-times (:around (fun n &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) args)
      (let* ((args (apply #'org-srs-review-cache-item args))
             (cached-due-times (org-srs-review-cache-due-times (org-srs-review-cache)))
             (due-times (gethash args cached-due-times)))
        (if (< (length due-times) n)
            (setf (gethash args cached-due-times) (apply fun n args))
          due-times))
    (apply fun n args)))

(define-advice org-srs-item-marker (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) args)
      (let ((args (apply #'org-srs-review-cache-item args))
            (markers (org-srs-review-cache-markers (org-srs-review-cache))))
        (org-srs-review-cache-ensure-gethash args markers (apply fun args)))
    (apply fun args)))

(define-advice org-srs-item-goto (:around (fun item &rest args) org-srs-review-cache)
  (if (or (bound-and-true-p org-srs-item-marker@org-srs-review-cache) (not (org-srs-review-cache-active-p)))
      (apply fun item args)
    (when args (cl-assert (eq (window-buffer) (current-buffer))))
    (defvar org-srs-item-marker@org-srs-review-cache)
    (org-srs-item-goto-marker (let ((org-srs-item-marker@org-srs-review-cache t)) (apply #'org-srs-item-marker item args)))))

(define-advice org-srs-query (:around (fun &rest args) org-srs-review-cache)
  (cl-destructuring-bind (predicate &optional (source (or (buffer-file-name) default-directory))) args
    (if (org-srs-review-cache-active-p)
        (let ((result (org-srs-review-cache-query predicate source)))
          (if (eq result org-srs-review-cache-null)
              (let* ((cache (or (org-srs-review-cache) (setf (org-srs-review-cache) (make-org-srs-review-cache :source source))))
                     (query (cl-multiple-value-bind (due-predicate rest-predicate) (org-srs-review-cache-query-predicate-due predicate)
                              (if due-predicate
                                  (let ((tree (org-srs-review-cache-pending-queue due-predicate)))
                                    (cl-flet* ((cache-marker (&optional (item (org-srs-review-cache-item)))
                                                 (setf (gethash item (org-srs-review-cache-markers cache)) (point-marker)))
                                               (cache-due-time (&aux (item (org-srs-review-cache-item)))
                                                 (avl-tree-enter tree (cons (org-srs-item-due-time) item))
                                                 (cache-marker item)
                                                 nil))
                                      `(and ,rest-predicate (or (and ,due-predicate ,#'cache-marker) ,#'cache-due-time))))
                                rest-predicate))))
                (setf (org-srs-review-cache-query predicate source) (funcall fun query source)))
            result))
      (funcall fun predicate source))))

(define-advice org-srs-review-start (:around (fun &rest args) org-srs-review-cache)
  (org-srs-property-let (org-srs-review-cache-p)
    (apply fun args)))

(define-advice org-srs-review-rate (:around (fun &rest args) org-srs-review-cache)
  (org-srs-property-let (org-srs-review-cache-p)
    (apply fun args)))

(defconst org-srs-review-cache-due-time-count 2
  "Number of due times to cache for review items.")

(defun org-srs-review-cache-updated-item (&rest args)
  "Update the review cache for the updated review item specified by ARGS."
  (when-let ((cache (org-srs-review-cache)) (item (apply #'org-srs-review-cache-item args)))
    (org-srs-review-cache-with-query-predicate-cache
      (org-srs-item-with-current item
        (cl-loop with due-time = (cl-first (setf (gethash item (org-srs-review-cache-due-times cache)) (org-srs-item-due-times org-srs-review-cache-due-time-count)))
                 for (predicate . items) in (org-srs-review-cache-queries cache)
                 for (due-predicate rest-predicate) = (cl-multiple-value-list (org-srs-review-cache-query-predicate-due predicate))
                 for (all-satisfied-p . rest-satisfied-p) = (if due-predicate
                                                                (when (funcall (org-srs-query-predicate rest-predicate))
                                                                  (cons (funcall (org-srs-query-predicate due-predicate)) t))
                                                              (cons (funcall (org-srs-query-predicate rest-predicate)) nil))
                 if all-satisfied-p do (setf (gethash item items) t)
                 else do (remhash item items)
                 when (and rest-satisfied-p (not all-satisfied-p))
                 do (avl-tree-enter (org-srs-review-cache-pending-queue due-predicate) (cons due-time item)))))))

(defvar org-srs-review-item)

(cl-defun org-srs-review-cache-before-rate (&optional (item org-srs-review-item))
  "Update the review cache before rating ITEM."
  (when (org-srs-review-cache-p)
    (cl-loop with item-due-time = (apply #'org-srs-item-due-time item)
             with item-element = (cons item-due-time (apply #'org-srs-review-cache-item item))
             for (due-predicate . tree) in (org-srs-review-cache-pending (org-srs-review-cache))
             for due-time = (org-srs-review-cache-query-predicate-due-time due-predicate)
             when (org-srs-time< due-time item-due-time)
             do (avl-tree-delete tree item-element))))

(add-hook 'org-srs-review-before-rate-hook #'org-srs-review-cache-before-rate)

(cl-defun org-srs-review-cache-after-rate (&optional (item org-srs-review-item))
  "Update the review cache after rating ITEM."
  (when (org-srs-review-cache-p)
    (apply #'org-srs-review-cache-updated-item item)))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-review-cache-after-rate 10)

(defun org-srs-review-cache-cleanup-on-quit ()
  "Clear the review cache when quitting a review session."
  (when (and (org-srs-review-cache-p) (not (org-srs-reviewing-p)))
    (org-srs-review-cache-clear)))

(add-hook 'org-srs-review-continue-hook #'org-srs-review-cache-cleanup-on-quit)
(add-hook 'org-srs-review-finish-hook #'org-srs-review-cache-cleanup-on-quit)

(define-advice \(setf\ org-srs-item-due-timestamp\) (:after (_ &rest args) org-srs-review-cache)
  (when (org-srs-review-cache-p)
    (apply #'org-srs-review-cache-updated-item args)))

(define-advice org-toggle-comment (:after (&rest _args) org-srs-review-cache)
  (when (org-srs-review-cache-active-p)
    (mapc
     (apply-partially #'apply #'org-srs-review-cache-updated-item)
     (org-srs-property-let ((org-srs-review-cache-p nil))
       (org-srs-query '(and) (cons (org-srs-entry-beginning-position) (org-srs-entry-end-position)))))))

(define-advice org-srs-query-predicate (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) (not (bound-and-true-p org-srs-query-predicate@org-srs-review-cache)))
      (cl-destructuring-bind (desc) args
        (cl-etypecase desc
          ((or list symbol)
           (cl-labels ((predicate (&rest args &aux (cache (org-srs-review-cache)))
                         (if-let ((items (cdr (cl-assoc desc (org-srs-review-cache-queries cache) :test #'equal)))
                                  (item (apply #'org-srs-review-cache-item args)))
                             (gethash item items)
                           (cl-assert (org-srs-review-cache-active-p))
                           (org-srs-query desc (org-srs-review-cache-source cache))
                           (apply #'predicate args))))
             #'predicate))
          (function desc)))
    (apply fun args)))

(defmacro org-srs-review-cache-without-query-predicate (&rest body)
  "Execute BODY without using predicates that utilize the review cache."
  (declare (indent 0))
  `(progn
     (defvar org-srs-query-predicate@org-srs-review-cache)
     (let ((org-srs-query-predicate@org-srs-review-cache t)) . ,body)))

(define-advice org-srs-review-cache-updated-item (:around (fun &rest args) org-srs-query-predicate@org-srs-review-cache)
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
