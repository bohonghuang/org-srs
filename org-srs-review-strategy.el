;;; org-srs-review-strategy.el --- Flexible review strategy framework -*- lexical-binding:t -*-

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

;; This package provides a flexible framework for defining and composing
;; review strategies in Org-srs.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-time)
(require 'org-srs-query)

(defgroup org-srs-review-strategy nil
  "Framework for defining and composing review strategies in Org-srs."
  :group 'org-srs-review
  :prefix "org-srs-review-strategy")

(org-srs-property-defcustom org-srs-review-strategy nil
  "Default review strategy to use."
  :group 'org-srs-review-strategy
  :type 'sexp)

(defvar org-srs-review-source)

(cl-defgeneric org-srs-review-strategy-items (type strategy &rest args))

(defvar org-srs-review-strategy-due-predicate 'due)

(cl-defmethod org-srs-review-strategy-items (type (strategy list) &rest args)
  (cl-assert (null args))
  (apply #'org-srs-review-strategy-items type strategy))

(defun org-srs-review-strategy-intersection (&rest args)
  (cl-loop with table = (make-hash-table :test #'equal)
           for items in args
           do (cl-loop for item in items
                       do (cl-incf (gethash item table 0) 1))
           finally (cl-return
                    (cl-loop with length = (length args)
                             for item being the hash-key of table using (hash-value count)
                             when (= count length)
                             collect item))))

(defun org-srs-review-strategy-difference (&rest args)
  (cl-loop with table = (make-hash-table :test #'equal)
           initially (cl-loop for item in (cl-first args)
                              do (setf (gethash item table) t))
           for items in (cl-rest args)
           do (cl-loop for item in items do (remhash item table))
           finally (cl-return (hash-table-keys table))))

(cl-defmethod org-srs-review-strategy-items (type (_strategy (eql 'union)) &rest strategies)
  (apply #'org-srs-review-strategy-union (mapcar (apply-partially #'org-srs-review-strategy-items type) strategies)))

(cl-defmethod org-srs-review-strategy-items (type (_strategy (eql 'intersection)) &rest strategies)
  (apply #'org-srs-review-strategy-intersection (mapcar (apply-partially #'org-srs-review-strategy-items type) strategies)))

(cl-defmethod org-srs-review-strategy-items (type (_strategy (eql 'difference)) &rest strategies)
  (apply #'org-srs-review-strategy-difference (mapcar (apply-partially #'org-srs-review-strategy-items type) strategies)))

(cl-defmethod org-srs-review-strategy-items ((type (eql 'todo)) (_strategy (eql 'or)) &rest strategies)
  (cl-loop for strategy in strategies thereis (org-srs-review-strategy-items type strategy)))

(cl-defmethod org-srs-review-strategy-items ((type (eql 'done)) (_strategy (eql 'or)) &rest strategies)
  (apply #'org-srs-review-strategy-items type 'union strategies))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'todo)) (_strategy (eql 'new)) &rest _args)
  (org-srs-query `(and ,org-srs-review-strategy-due-predicate new (not suspended)) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'done)) (_strategy (eql 'new)) &rest _args)
  (org-srs-query 'learned org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'todo)) (_strategy (eql 'old)) &rest _args)
  (org-srs-query `(and ,org-srs-review-strategy-due-predicate (not new) (not reviewed) (not suspended)) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'done)) (_strategy (eql 'old)) &rest _args)
  (org-srs-query '(and (not learned) reviewed) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'todo)) (_strategy (eql 'reviewing)) &rest _args)
  (org-srs-query `(and ,org-srs-review-strategy-due-predicate reviewed) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'done)) (strategy (eql 'reviewing)) &rest args)
  (org-srs-review-strategy-difference
   (org-srs-query 'reviewed org-srs-review-source)
   (apply #'org-srs-review-strategy-items 'todo strategy args)))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'todo)) (_strategy (eql 'limit)) &rest args)
  (cl-destructuring-bind (strategy limit) args
    (when (< (length (org-srs-review-strategy-items 'done strategy)) limit)
      (org-srs-review-strategy-items 'todo strategy))))

(cl-defmethod org-srs-review-strategy-items ((type (eql 'done)) (_strategy (eql 'limit)) &rest args)
  (cl-destructuring-bind (strategy _limit) args
    (org-srs-review-strategy-items type strategy)))

(cl-defmethod org-srs-review-strategy-items ((type (eql 'todo)) (_strategy (eql 'subseq)) &rest args)
  (cl-destructuring-bind (strategy &optional (start 0) (end 1)) args
    (let ((items (org-srs-review-strategy-items type strategy)))
      (cl-subseq items start (min end (length items))))))

(cl-defmethod org-srs-review-strategy-items ((type (eql 'done)) (_strategy (eql 'subseq)) &rest args)
  (cl-destructuring-bind (strategy &rest args) args
    (org-srs-review-strategy-items type strategy)))

(cl-defmethod org-srs-review-strategy-items (_type (_strategy (eql 'done)) &rest args)
  (cl-destructuring-bind (strategy) args
    (org-srs-review-strategy-items 'done strategy)))

(defun org-srs-review-strategy-union (&rest args)
  (cl-loop with table = (make-hash-table :test #'equal)
           for items in args
           do (cl-loop for item in items do (setf (gethash item table) t))
           finally (cl-return (hash-table-keys table))))

(cl-defmethod org-srs-review-strategy-items (type (_strategy (eql 'ahead)) &rest args)
  (cl-destructuring-bind (strategy &optional (time (org-srs-time-tomorrow))) args
    (let ((org-srs-review-strategy-due-predicate `(due ,time)))
      (cl-assert (org-srs-time< (org-srs-time-now) (org-srs-time-tomorrow)))
      (org-srs-review-strategy-items type strategy))))

(defun org-srs-review-strategy-item-marker< (marker-a marker-b)
  (let ((buffer-a (marker-buffer marker-a))
        (buffer-b (marker-buffer marker-b)))
    (if (eq buffer-a buffer-b)
        (< marker-a marker-b)
      (let ((name-a (or (buffer-file-name buffer-a) (buffer-name buffer-a)))
            (name-b (or (buffer-file-name buffer-b) (buffer-name buffer-b))))
        (string< name-a name-b)))))

(cl-defmethod org-srs-review-strategy-items (type (_strategy (eql 'sort)) &rest args)
  (cl-destructuring-bind (strategy order &rest args &aux (items (org-srs-review-strategy-items type strategy))) args
    (cl-case order
      (position (cl-sort items #'org-srs-review-strategy-item-marker< :key (apply-partially #'apply #'org-srs-item-marker)))
      (due-date (cl-sort items #'org-srs-time< :key (apply-partially #'apply #'org-srs-item-due-time)))
      (priority (cl-sort items #'> :key (apply-partially #'apply #'org-srs-item-priority)))
      (random (cl-sort items #'< :key #'sxhash-eq))
      (t (cl-sort items #'< :key order)))))

(provide 'org-srs-review-strategy)
;;; org-srs-review-strategy.el ends here
