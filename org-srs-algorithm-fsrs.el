;;; org-srs-algorithm-fsrs.el --- FSRS algorithm integration for Org-srs -*- lexical-binding:t -*-

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

;; This package serves as the algorithm interface for Org-srs, allowing
;; the core to be independent of specific algorithms.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)

(require 'fsrs)

(require 'org-srs-algorithm)
(require 'org-srs-time)
(require 'org-srs-schedule-step)

(cl-defmethod org-srs-algorithm-ensure ((_type (eql 'fsrs)) &rest args)
  (apply #'fsrs-make-scheduler args))

(cl-defmethod org-srs-algorithm-repeat ((_fsrs fsrs-scheduler) (_args null))
  (let ((card (fsrs-make-card)))
    `((stability . ,(fsrs-card-stability card))
      (difficulty . ,(fsrs-card-difficulty card))
      (state . ,(fsrs-card-state card)))))

(defconst org-srs-algorithm-fsrs-card-slots (mapcar #'cl--slot-descriptor-name (cl--class-slots (cl-find-class 'fsrs-card))))

(defun org-srs-algorithm-fsrs-ensure-card (object)
  (cl-etypecase object
    (fsrs-card object)
    (list (cl-loop with card = (fsrs-make-card) and args = (copy-alist object)
                   initially (setf (car (cl-find 'timestamp args :key #'car :from-end t)) 'last-review)
                   for slot in org-srs-algorithm-fsrs-card-slots
                   for cons = (assoc slot args)
                   for (key . value) = cons
                   when cons
                   do (setf (eieio-oref card key) value)
                   finally (cl-return card)))))

(cl-defmethod org-srs-algorithm-repeat ((fsrs fsrs-scheduler) (args list))
  (cl-loop with card-old = (org-srs-algorithm-fsrs-ensure-card args)
           and rating = (alist-get 'rating args)
           and timestamp = (alist-get 'timestamp args (org-srs-timestamp-now))
           with card-new = (cl-nth-value 0 (fsrs-scheduler-review-card fsrs card-old rating timestamp))
           for slot in org-srs-algorithm-fsrs-card-slots
           collect (cons
                    (cl-case slot
                      (due 'timestamp)
                      (t slot))
                    (cl-case slot
                      (state (or (org-srs-schedule-step-state) (fsrs-card-state card-new)))
                      (t (eieio-oref card-new slot))))
           into slots
           finally (cl-return (nconc slots args))))

(provide 'org-srs-algorithm-fsrs)
;;; org-srs-algorithm-fsrs.el ends here
