;;; org-srs-step.el --- Stepped (re)learning mechanism  -*- lexical-binding:t -*-

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

;; This package replicates the stepped (re)learning feature in Anki,
;; allowing the (re)learning process to be divided into specified
;; intervals to enhance learning effectiveness.

;;; Code:

(require 'cl-lib)

(require 'org-srs-property)
(require 'org-srs-review)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-time)

(org-srs-property-defcustom org-srs-step-learning-steps '((1 :minute) (10 :minute))
  "The number of learning repetitions, and the delay between them."
  :group 'org-srs
  :type 'sexp)

(org-srs-property-defcustom org-srs-step-relearning-steps '((10 :minute))
  "Same as variable `org-srs-step-learning-steps', but for items being relearned."
  :group 'org-srs
  :type 'sexp)

(defun org-srs-step-learning-relearning ()
  (save-excursion
    (cl-loop with step = 1 and learning-step = 0 and relearning-step = 0
             for field = (org-srs-table-field 'rating)
             for rating = (if (string-empty-p field)
                              (cl-return (cl-values (max step learning-step) relearning-step))
                            (read field))
             do (cl-ecase rating
                  (:easy
                   (cl-return (cl-values most-positive-fixnum most-positive-fixnum)))
                  (:good
                   (setf step (truncate (1+ step))))
                  (:hard
                   (when (= (truncate step) 1) (setf step 1.5)))
                  (:again
                   (when (zerop relearning-step) (setf relearning-step step))
                   (setf learning-step (max learning-step (cl-shiftf step 1)))))
             until (cl-minusp (forward-line -1))
             until (org-at-table-hline-p))))

(cl-defun org-srs-step-due-timestamp ()
  (save-excursion
    (let ((timestamp-scheduled (org-srs-table-field 'timestamp))
          (timestamp-review (progn (forward-line -1) (org-srs-table-field 'timestamp))))
      (cl-multiple-value-bind (learning-step relearning-step) (org-srs-step-learning-relearning)
        (cl-multiple-value-bind (steps step)
            (let ((learning-steps (org-srs-step-learning-steps)))
              (cond
               ((zerop relearning-step)
                (cl-values learning-steps learning-step))
               ((< (length learning-steps) learning-step)
                (let ((relearning-steps (org-srs-step-relearning-steps)))
                  (cl-assert (<= (length relearning-steps) (length learning-steps)))
                  (cl-values relearning-steps relearning-step)))
               (t
                (cl-values learning-steps relearning-step))))
          (cl-assert (cl-plusp step))
          (cl-multiple-value-bind (step frac) (cl-truncate step)
            (let* ((index (1- step))
                   (index-next (if (< (abs frac) 1e-3) index (1+ index))))
              (unless (< index (length steps))
                (cl-return-from org-srs-step-due-timestamp timestamp-scheduled))
              (if (< index-next (length steps))
                  (when-let ((step (nth index steps))
                             (step-next (nth index-next steps)))
                    (cl-assert (= (length step) (length step-next) 2))
                    (let ((step (cons (* (car step) frac) (cdr step)))
                          (step-next (cons (* (car step-next) (- 1.0 frac)) (cdr step))))
                      (apply #'org-srs-timestamp+ (apply #'org-srs-timestamp+ timestamp-review step) step-next)))
                (let* ((step-last (car (last steps)))
                       (step-next (cons (* 1.5 (car step-last)) (cdr step-last))))
                  (org-srs-timestamp-min
                   (apply #'org-srs-timestamp+ timestamp-review step-next)
                   (org-srs-timestamp+ (apply #'org-srs-timestamp+ timestamp-review step-last) 1 :day)))))))))))

(defun org-srs-step-update-due-timestamp ()
  (save-excursion
    (goto-char org-srs-review-item-marker)
    (org-srs-table-goto-starred-line)
    (setf (org-srs-table-field 'timestamp) (org-srs-step-due-timestamp))
    (org-table-align)
    (org-srs-log-hide-drawer)))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-step-update-due-timestamp 50)

(provide 'org-srs-step)
;;; org-srs-step.el ends here
