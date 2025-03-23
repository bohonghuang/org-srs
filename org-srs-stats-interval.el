;;; org-srs-stats-interval.el --- Repetition interval calculation -*- lexical-binding:t -*-

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

;; This package is used to calculate the time interval until the next
;; review for a review item under different ratings.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)
(require 'custom)

(require 'org-srs-review)
(require 'org-srs-review-cache)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-time)

(cl-defgeneric org-srs-stats-interval-deep-copy (object) object)

(cl-defmethod org-srs-stats-interval-deep-copy ((null null)) null)

(cl-defmethod org-srs-stats-interval-deep-copy ((cons cons))
  (cons (org-srs-stats-interval-deep-copy (car cons)) (org-srs-stats-interval-deep-copy (cdr cons))))

(cl-defmethod org-srs-stats-interval-deep-copy ((vector vector))
  (cl-map (cl-type-of vector) #'org-srs-stats-interval-deep-copy vector))

(cl-defmethod org-srs-stats-interval-deep-copy ((object cl-structure-object))
  (cl-loop with new-object = (copy-sequence object)
           for slot in (mapcar #'cl--slot-descriptor-name (cl--class-slots (cl-find-class (cl-type-of object))))
           do (setf (eieio-oref new-object slot) (org-srs-stats-interval-deep-copy (eieio-oref object slot)))
           finally (cl-return new-object)))

(cl-defun org-srs-stats-interval-1 (rating)
  (org-srs-property-let ((org-srs-review-cache-query-p nil))
    (defvar org-srs-time-now)
    (let ((org-srs-time-now (cl-constantly (org-srs-time-now))))
      (save-excursion
        (org-srs-table-goto-starred-line)
        (org-srs-table-with-temp-buffer-1
          (make-local-variable 'org-srs-review-item-marker)
          (save-excursion
            (goto-char (point-min))
            (open-line 1)
            (insert "* HEADLINE"))
          (defvar cl--random-state)
          (let ((cl--random-state (org-srs-stats-interval-deep-copy cl--random-state)))
            (org-srs-log-repeat rating)
            (defvar org-srs-review-rating)
            (defvar org-srs-review-item-marker)
            (let ((org-srs-review-rating rating)
                  (org-srs-review-item-marker (point-marker)))
              (cl-assert (not (local-variable-p 'org-srs-review-after-rate-hook)))
              (run-hooks 'org-srs-review-after-rate-hook))
            (cl-return-from org-srs-stats-interval-1
              (org-srs-timestamp-difference
               (org-srs-item-due-timestamp)
               (org-srs-timestamp-now)))))))))

(defun org-srs-stats-interval (rating)
  (org-srs-property-let t
    (org-srs-stats-interval-1 rating)))

(cl-defun org-srs-stats-intervals (&optional (ratings org-srs-review-ratings))
  (org-srs-property-let t
    (cl-loop for rating in ratings
             nconc (list rating (org-srs-stats-interval-1 rating)))))

(provide 'org-srs-stats-interval)
;;; org-srs-stats-interval.el ends here
