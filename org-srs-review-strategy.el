;;; org-srs-review-strategy.el --- Flexible review strategy framework -*- lexical-binding: t -*-

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

;; This package provides a flexible framework for defining and composing
;; review strategies in Org-srs.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'pcase)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-time)
(require 'org-srs-query)

(defgroup org-srs-review-strategy nil
  "Framework for defining and composing review strategies in Org-srs."
  :group 'org-srs-review
  :prefix "org-srs-review-strategy")

(defun org-srs-review-strategy-ensure (object)
  "Ensure OBJECT is a valid review strategy."
  (cl-typecase object
    (function (funcall object))
    (t object)))

(org-srs-property-defcustom org-srs-review-strategy nil
  "Default review strategy to use."
  :group 'org-srs-review-strategy
  :transform #'org-srs-review-strategy-ensure
  :type 'sexp)

(defvar org-srs-review-source)

(cl-defgeneric org-srs-review-strategy-items (state strategy &rest args)
  (:documentation "Return review items in STATE matching STRATEGY with ARGS."))

(defvar org-srs-review-strategy-due-predicate 'due
  "Predicate used in review strategies to determine if a review item is due.")

(cl-defstruct org-srs-review-strategy-class-todo)

(cl-defmethod org-srs-review-strategy-items ((_state (eql 'todo)) strategy &rest args)
  "Method to return items in state `todo' matching STRATEGY with ARGS."
  (apply #'org-srs-review-strategy-items '#s(org-srs-review-strategy-class-todo) strategy args))

(cl-defstruct org-srs-review-strategy-class-done)

(cl-defmethod org-srs-review-strategy-items ((_state (eql 'done)) strategy &rest args)
  "Method to return items in state `done' matching STRATEGY with ARGS."
  (apply #'org-srs-review-strategy-items '#s(org-srs-review-strategy-class-done) strategy args))

(cl-defstruct (org-srs-review-strategy-class-todoset (:include org-srs-review-strategy-class-todo)))

(cl-defmethod org-srs-review-strategy-items ((_state (eql 'todoset)) strategy &rest args)
  "Method to return prospective items in state `todo' matching STRATEGY with ARGS."
  (apply #'org-srs-review-strategy-items '#s(org-srs-review-strategy-class-todoset) strategy args))

(cl-defmethod org-srs-review-strategy-items (state (strategy list) &rest args)
  "Method to return items in STATE for STRATEGY with ARGS ignored."
  (cl-assert (null args))
  (apply #'org-srs-review-strategy-items state strategy))

(defun org-srs-review-strategy-intersection (&rest args)
  "Return the intersection of multiple review item lists in ARGS."
  (org-srs-query-with-loop
    (cl-loop with table = (make-hash-table :test #'equal)
             and length = (length args)
             initially (cl-loop for items in args
                                do (cl-loop for item in items
                                            do (cl-incf (gethash item table 0) 1)))
             for item being the hash-key of table using (hash-value count)
             when (= count length)
             collect item)))

(defun org-srs-review-strategy-multiply (&rest args)
  "Like `org-srs-review-strategy-intersection', but preserve item orders in ARGS."
  (org-srs-query-with-loop
    (cl-loop with table = (make-hash-table :test #'equal)
             and length = (length args)
             initially (cl-loop for items in args
                                do (cl-loop for item in items
                                            do (cl-incf (gethash item table 0) 1)))
             for items in args
             nconc (cl-loop for item in items
                            when (eql (gethash item table) length)
                            collect item and do (remhash item table)))))

(defun org-srs-review-strategy-union (&rest args)
  "Return the union of multiple review item lists in ARGS."
  (cl-loop with table = (make-hash-table :test #'equal)
           for items in args
           do (cl-loop for item in items do (setf (gethash item table) t))
           finally (cl-return (hash-table-keys table))))

(defun org-srs-review-strategy-append (&rest args)
  "Like `org-srs-review-strategy-union', but preserve item orders in ARGS."
  (org-srs-query-with-loop
    (cl-loop with table = (make-hash-table :test #'equal)
             for items in args
             nconc (cl-loop for item in items
                            unless (gethash item table)
                            do (setf (gethash item table) t)
                            and collect item))))

(defun org-srs-review-strategy-difference (&rest args)
  "Return review items in ARGS' first list excluding those in the rest."
  (cl-loop with table = (make-hash-table :test #'equal)
           initially (cl-loop for item in (cl-first args) do (setf (gethash item table) t))
           for items in (cl-rest args)
           do (cl-loop for item in items do (remhash item table))
           finally (cl-return (hash-table-keys table))))

(defun org-srs-review-strategy-subtract (&rest args)
  "Like `org-srs-review-strategy-difference', but preserve item orders in ARGS."
  (org-srs-query-with-loop
    (cl-loop with table = (make-hash-table :test #'equal)
             initially (cl-loop for items in (cl-rest args)
                                do (cl-loop for item in items
                                            do (setf (gethash item table) t)))
             for item in (cl-first args)
             unless (gethash item table)
             collect item)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql 'union)) &rest strategies)
  "Method to return items in STATE matching any of STRATEGIES using logical union."
  (apply #'org-srs-review-strategy-union (mapcar (apply-partially #'org-srs-review-strategy-items state) strategies)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql 'intersection)) &rest strategies)
  "Method to return items in STATE common to STRATEGIES using logical intersection."
  (apply #'org-srs-review-strategy-intersection (mapcar (apply-partially #'org-srs-review-strategy-items state) strategies)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql 'difference)) &rest strategies)
  "Method to return items in STATE matching STRATEGIES' first excluding the rest."
  (apply #'org-srs-review-strategy-difference (mapcar (apply-partially #'org-srs-review-strategy-items state) strategies)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql '+)) &rest strategies)
  "Method to return items in STATE matching any of STRATEGIES using logical union.

Like strategy `union', but preserve order of items in STRATEGIES."
  (apply #'org-srs-review-strategy-append (mapcar (apply-partially #'org-srs-review-strategy-items state) strategies)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql '*)) &rest strategies)
  "Method to return items in STATE common to STRATEGIES using logical intersection.

Like strategy `intersection', but preserve orders of items in STRATEGIES."
  (apply #'org-srs-review-strategy-multiply (mapcar (apply-partially #'org-srs-review-strategy-items state) strategies)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql '-)) &rest strategies)
  "Method to return items in STATE matching STRATEGIES' first excluding the rest.

Like strategy `difference', but preserve orders of items in STRATEGIES."
  (apply #'org-srs-review-strategy-subtract (mapcar (apply-partially #'org-srs-review-strategy-items state) strategies)))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-todo) (_strategy (eql 'or)) &rest strategies)
  "Method to return items in STATE `todo' matching any of STRATEGIES."
  (cl-loop for strategy in strategies thereis (org-srs-review-strategy-items state strategy)))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-done) (_strategy (eql 'or)) &rest strategies)
  "Method to return items in STATE `done' matching any of STRATEGIES."
  (apply #'org-srs-review-strategy-items state 'union strategies))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-todoset) (_strategy (eql 'or)) &rest strategies)
  "Method to return all items in STATE `todo' matching STRATEGIES."
  (apply #'org-srs-review-strategy-items state 'union strategies))

(cl-defmethod org-srs-review-strategy-items ((_state org-srs-review-strategy-class-todo) (_strategy (eql 'due)) &rest _args)
  "Method to return all due items to be reviewed."
  (org-srs-query `(and ,org-srs-review-strategy-due-predicate (not reviewed) (not suspended)) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_state org-srs-review-strategy-class-done) (_strategy (eql 'due)) &rest _args)
  "Method to return all reviewed items no longer due."
  (org-srs-review-strategy-difference
   (org-srs-query 'reviewed org-srs-review-source)
   (org-srs-review-strategy-items 'todo 'due)))

(cl-defmethod org-srs-review-strategy-items ((_state org-srs-review-strategy-class-todo) (_strategy (eql 'new)) &rest _args)
  "Method to return all new items to be reviewed."
  (org-srs-query `(and ,org-srs-review-strategy-due-predicate new (not suspended)) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_state org-srs-review-strategy-class-done) (_strategy (eql 'new)) &rest _args)
  "Method to return all new items that have been reviewed."
  (org-srs-query 'learned org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_state org-srs-review-strategy-class-todo) (_strategy (eql 'old)) &rest _args)
  "Method to return all old items to be reviewed."
  (org-srs-query `(and ,org-srs-review-strategy-due-predicate (not new) (not reviewed) (not suspended)) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_state org-srs-review-strategy-class-done) (_strategy (eql 'old)) &rest _args)
  "Method to return all old items that have been reviewed."
  (org-srs-query '(and (not learned) reviewed) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_state org-srs-review-strategy-class-todo) (_strategy (eql 'reviewing)) &rest _args)
  "Method to return all items being reviewed but not yet completed."
  (org-srs-query `(and ,org-srs-review-strategy-due-predicate reviewed) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_state org-srs-review-strategy-class-done) (_strategy (eql 'reviewing)) &rest _args)
  "Method to return all items that have been reviewed."
  (org-srs-review-strategy-difference
   (org-srs-query 'reviewed org-srs-review-source)
   (org-srs-review-strategy-items 'todo 'reviewing)))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-todo) (_strategy (eql 'limit)) &rest args)
  "Method to return limited items in STATE `todo' matching the strategy in ARGS."
  (cl-destructuring-bind (strategy limit) args
    (when (< (length (org-srs-review-strategy-items 'done strategy)) limit)
      (org-srs-review-strategy-items state strategy))))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-done) (_strategy (eql 'limit)) &rest args)
  "Method to return all items in STATE `done' matching the strategy in ARGS."
  (cl-destructuring-bind (strategy _limit) args
    (org-srs-review-strategy-items state strategy)))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-todoset) (_strategy (eql 'limit)) &rest args)
  "Method to return all items in STATE `todo' matching the strategy in ARGS."
  (cl-destructuring-bind (strategy limit) args
    (let ((items (org-srs-review-strategy-items state strategy)))
      (cl-subseq items 0 (min (max (- limit (length (org-srs-review-strategy-items 'done strategy))) 0) (length items))))))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-todo) (_strategy (eql 'subseq)) &rest args)
  "Method to return partial items in STATE `todo' matching the strategy in ARGS."
  (cl-destructuring-bind (strategy &optional (start 0) (end 1)) args
    (let ((items (org-srs-review-strategy-items state strategy)))
      (cl-subseq items start (min end (length items))))))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-done) (_strategy (eql 'subseq)) &rest args)
  "Method to return all items in STATE `done' matching the strategy in ARGS."
  (cl-destructuring-bind (strategy &rest args) args
    (ignore args)
    (org-srs-review-strategy-items state strategy)))

(cl-defmethod org-srs-review-strategy-items ((state org-srs-review-strategy-class-todoset) (_strategy (eql 'subseq)) &rest args)
  "Method to return all items in STATE `todo' matching the strategy in ARGS."
  (cl-destructuring-bind (strategy &rest args) args
    (ignore args)
    (org-srs-review-strategy-items state strategy)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql 'reverse)) &rest args)
  "Method to return reversed items in STATE matching the strategy in ARGS."
  (cl-destructuring-bind (strategy) args
    (nreverse (org-srs-review-strategy-items state strategy))))

(cl-defmethod org-srs-review-strategy-items (_state (_strategy (eql 'done)) &rest args)
  "Method to return all reviewed items matching the strategy in ARGS."
  (cl-destructuring-bind (strategy) args
    (org-srs-review-strategy-items 'done strategy)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql 'ahead)) &rest args)
  "Method to return items in STATE due in the future matching the strategy in ARGS."
  (cl-destructuring-bind (strategy &optional (time (org-srs-time-tomorrow))) args
    (let ((org-srs-review-strategy-due-predicate `(due ,time)))
      (cl-assert (org-srs-time< (org-srs-time-now) (org-srs-time-tomorrow)))
      (org-srs-review-strategy-items state strategy))))

(defun org-srs-review-strategy-item-marker< (marker-a marker-b)
  "Return non-nil if MARKER-A appears before MARKER-B in buffer and position order."
  (let ((buffer-a (marker-buffer marker-a))
        (buffer-b (marker-buffer marker-b)))
    (if (eq buffer-a buffer-b)
        (< marker-a marker-b)
      (let ((name-a (or (buffer-file-name buffer-a) (buffer-name buffer-a)))
            (name-b (or (buffer-file-name buffer-b) (buffer-name buffer-b))))
        (string< name-a name-b)))))

(cl-defun org-srs-review-strategy-sort (items order &key (key #'identity))
  "Sort ITEMS according to ORDER and KEY."
  (apply
   #'cl-sort items
   (let ((args (pcase-exhaustive order
                 ('position (list #'org-srs-review-strategy-item-marker< :key (apply-partially #'apply #'org-srs-item-marker)))
                 ('due-date (list #'org-srs-time< :key (apply-partially #'apply #'org-srs-item-due-time)))
                 ('priority (list #'> :key (apply-partially #'apply #'org-srs-item-priority)))
                 ('interval (list #'< :key (apply-partially #'apply #'org-srs-item-interval)))
                 ('random (list #'< :key #'sxhash-eq))
                 (`(reverse ,order) (cl-return-from org-srs-review-strategy-sort (nreverse (org-srs-review-strategy-sort items order :key key))))
                 (`(,(pred functionp) . ,(pred plistp)) order)
                 ((pred functionp)
                  (pcase-exhaustive (car (func-arity order))
                    (2 (list order))
                    ((or 0 1) (list #'< :key order)))))))
     (unless (eq key #'identity)
       (cl-callf (lambda (accessor) (lambda (object) (funcall accessor (funcall key object)))) (cl-getf (cdr args) :key)))
     args)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql 'sort)) &rest args)
  "Method to return sorted items in STATE matching the strategy in ARGS."
  (cl-destructuring-bind (strategy order &key args) args
    (apply #'org-srs-review-strategy-sort (org-srs-review-strategy-items state strategy) order args)))

(cl-defmethod org-srs-review-strategy-items (state (_strategy (eql 'sort-append)) &optional order &rest strategies)
  "Method to sort and concatenate items in STATE from STRATEGIES by ORDER."
  (mapcan #'identity (org-srs-review-strategy-sort (mapcar (apply-partially #'apply #'org-srs-review-strategy-items state) strategies) order :key #'cl-first)))

(provide 'org-srs-review-strategy)
;;; org-srs-review-strategy.el ends here
