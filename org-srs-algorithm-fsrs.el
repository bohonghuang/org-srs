;;; org-srs-algorithm-fsrs.el --- The FSRS algorithm integration for Org-srs -*- lexical-binding:t -*-

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

;; This package serves as the algorithm interface for Org-srs, allowing
;; the core to be independent of specific algorithms.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)

(require 'fsrs)

(require 'org-srs-algorithm)
(require 'org-srs-time)
(require 'org-srs-step)

(cl-defmethod org-srs-algorithm-ensure ((_type (eql 'fsrs)) &rest args)
  (apply #'make-fsrs-scheduler args))

(cl-defmethod org-srs-algorithm-repeat ((_fsrs fsrs-scheduler) (_args null))
  (let ((card (make-fsrs-card)))
    `((stability . ,(fsrs-card-stability card))
      (difficulty . ,(fsrs-card-difficulty card))
      (state . ,(fsrs-card-state card)))))

(defconst org-srs-algorithm-fsrs-card-slots (mapcar #'cl--slot-descriptor-name (cl--class-slots (cl-find-class 'fsrs-card))))

(cl-defmethod org-srs-algorithm-repeat ((fsrs fsrs-scheduler) (args list))
  (let ((card (make-fsrs-card))
        (rating (alist-get 'rating args))
        (timestamp (alist-get 'timestamp args (org-srs-timestamp-now))))
    (cl-assert (keywordp rating))
    (setf (car (cl-find 'timestamp args :key #'car :from-end t)) 'last-review)
    (cl-loop for slot in org-srs-algorithm-fsrs-card-slots
             for cons = (assoc slot args)
             for (key . value) = cons
             when cons
             do (setf (eieio-oref card key) value))
    (cl-loop with card = (cl-nth-value 0 (fsrs-scheduler-review-card fsrs card rating timestamp))
             for slot in org-srs-algorithm-fsrs-card-slots
             collect (cons
                      (cl-case slot (due 'timestamp) (t slot))
                      (cl-case slot (state (or (org-srs-step-state) (fsrs-card-state card))) (t (eieio-oref card slot))))
             into slots
             finally (cl-return (nconc slots args)))))

(provide 'org-srs-algorithm-fsrs)
;;; org-srs-algorithm-fsrs.el ends here
