;;; org-srs-stats.el --- Interface and common utilities for status statistics -*- lexical-binding:t -*-

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

;; This package serves as the interface and provides common utilities for
;; statistics to monitor learning status.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)
(require 'custom)

(require 'org)

(require 'org-srs-review)
(require 'org-srs-review-cache)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-time)

(defgroup org-srs-stats nil
  "Interface for status statistics."
  :group 'org-srs
  :prefix "org-srs-stats-")

(cl-defgeneric org-srs-stats-deep-copy (object) object)

(cl-defmethod org-srs-stats-deep-copy ((null null)) null)

(cl-defmethod org-srs-stats-deep-copy ((cons cons))
  (cons (org-srs-stats-deep-copy (car cons)) (org-srs-stats-deep-copy (cdr cons))))

(cl-defmethod org-srs-stats-deep-copy ((vector vector))
  (cl-map (cl-type-of vector) #'org-srs-stats-deep-copy vector))

(cl-defmethod org-srs-stats-deep-copy ((object cl-structure-object))
  (cl-loop with new-object = (copy-sequence object)
           for slot in (mapcar #'cl--slot-descriptor-name (cl--class-slots (cl-find-class (cl-type-of object))))
           do (setf (eieio-oref new-object slot) (org-srs-stats-deep-copy (eieio-oref object slot)))
           finally (cl-return new-object)))

(cl-defun org-srs-stats-call-with-rating-simulator (thunk)
  (org-srs-property-let org-srs-algorithm
    (org-srs-property-let org-srs-schedule
      (org-srs-property-let ((org-srs-review-cache-p nil))
        (defvar org-srs-time-now)
        (let ((org-srs-time-now (cl-constantly (org-srs-time-now)))
              (org-srs-table-with-temp-buffer-function #'funcall)
              (org-table-automatic-realign nil))
          (save-excursion
            (org-srs-table-goto-starred-line)
            (org-srs-table-with-temp-buffer-1
              (make-local-variable 'org-srs-review-item-marker)
              (save-excursion
                (goto-char (point-min))
                (open-line 1)
                (insert "* HEADLINE"))
              (org-srs-table-duplicate-line)
              (defvar cl--random-state)
              (let ((cl--random-state (org-srs-stats-deep-copy cl--random-state))
                    (buffer-undo-list nil))
                (defvar org-srs-review-rating)
                (defvar org-srs-review-item-marker)
                (cl-flet ((rate (rating)
                            (save-excursion
                              (prog2 (let ((org-srs-review-rating rating)
                                           (org-srs-review-item-marker (point-marker)))
                                       (undo-boundary)
                                       (cl-assert (not (local-variable-p 'org-srs-review-before-rate-hook)))
                                       (cl-assert (not (local-variable-p 'org-srs-review-after-rate-hook)))
                                       (run-hooks 'org-srs-review-before-rate-hook)
                                       (org-srs-log-repeat rating)
                                       (run-hooks 'org-srs-review-after-rate-hook))
                                  (org-srs-timestamp-difference
                                   (org-srs-item-due-timestamp)
                                   (org-srs-timestamp-now))
                                (primitive-undo 1 buffer-undo-list)))))
                  (cl-return-from org-srs-stats-call-with-rating-simulator (funcall thunk #'rate)))))))))))

(cl-defmacro org-srs-stats-with-rating-simulator (args &rest body)
  (declare (indent 1))
  `(org-srs-stats-call-with-rating-simulator
    (lambda ,args
      (cl-flet ,(cl-loop for arg in args collect (cl-with-gensyms (args) `(,arg (&rest ,args) (apply ,arg ,args))))
        ,@body))))

(provide 'org-srs-stats)
;;; org-srs-stats.el ends here
