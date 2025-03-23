;;; org-srs-step.el --- Stepped (re)learning mechanism  -*- lexical-binding:t -*-

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

;; This package replicates the stepped (re)learning feature in Anki,
;; allowing the (re)learning process to be divided into specified
;; intervals to enhance learning effectiveness.

;;; Code:

(require 'cl-lib)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-review)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-time)

(defgroup org-srs-step nil
  "Stepped learning mechanism to ensure retention before reviews."
  :group 'org-srs
  :prefix "org-srs-step-")

(org-srs-property-defcustom org-srs-step-learning-steps '((1 :minute) (10 :minute))
  "Number of learning repetitions, and the delay between them."
  :group 'org-srs-step
  :type 'sexp)

(org-srs-property-defcustom org-srs-step-relearning-steps '((10 :minute))
  "Same as variable `org-srs-step-learning-steps', but for items being relearned."
  :group 'org-srs-step
  :type 'sexp)

(defun org-srs-step-list ()
  (save-excursion
    (cl-loop with step-max = (1- most-positive-fixnum) and steps = nil
             initially (setf step 1)
             for field = (org-srs-table-field 'rating)
             for rating = (if (string-empty-p field) :again (read field))
             for step = (cl-ecase rating
                          (:easy step-max)
                          (:good (min (truncate (1+ step)) step-max))
                          (:hard (if (= (truncate step) 1) 1.5 step))
                          (:again (setf steps (list step)) 1))
             nconc (cl-shiftf steps nil)
             until (cl-minusp (forward-line -1))
             until (org-at-table-hline-p))))

(cl-defun org-srs-step-learned-p (&optional (learning-steps (org-srs-step-learning-steps)) (step-list (org-srs-step-list)))
  (cl-some (apply-partially #'< (length learning-steps)) (cl-rest step-list)))

(defun org-srs-step-steps ()
  (let ((step-list (org-srs-step-list))
        (learning-steps (org-srs-step-learning-steps)))
    (cl-assert (cl-plusp (length step-list)))
    (cl-values
     (cl-first step-list)
     (if (cl-some (apply-partially #'< (length learning-steps)) (cl-rest step-list))
         (org-srs-step-relearning-steps)
       learning-steps))))

(defun org-srs-step-state ()
  (org-srs-property-let (org-srs-step-learning-steps org-srs-step-relearning-steps)
    (let ((learning-steps (org-srs-step-learning-steps))
          (relearning-steps (org-srs-step-relearning-steps)))
      (save-excursion
        (cl-loop while (org-at-table-p)
                 while (string-empty-p (org-srs-table-field 'rating))
                 until (cl-minusp (forward-line -1))
                 until (org-at-table-hline-p))
        (cl-multiple-value-bind (step steps) (org-srs-step-steps)
          (unless (> step (length steps))
            (if (eq steps learning-steps) :learning
              (cl-assert (eq steps relearning-steps))
              :relearning)))))))

(cl-defun org-srs-step-due-timestamp ()
  (save-excursion
    (let ((timestamp-scheduled (org-srs-table-field 'timestamp))
          (timestamp-review (progn (forward-line -1) (org-srs-table-field 'timestamp))))
      (cl-multiple-value-bind (step steps) (org-srs-step-steps)
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
                 (org-srs-timestamp+ (apply #'org-srs-timestamp+ timestamp-review step-last) 1 :day))))))))))

(defun org-srs-step-update-due-timestamp ()
  (if (boundp 'org-srs-review-rating)
      (when (symbol-value 'org-srs-review-rating)
        (goto-char org-srs-review-item-marker)
        (org-srs-table-goto-starred-line)
        (org-srs-property-let (org-srs-step-learning-steps org-srs-step-relearning-steps)
          (org-srs-table-with-temp-buffer
            (setf (org-srs-table-field 'timestamp) (org-srs-step-due-timestamp)))))
    (setf (org-srs-table-field 'timestamp) (org-srs-step-due-timestamp))))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-step-update-due-timestamp 50)

(provide 'org-srs-step)
;;; org-srs-step.el ends here
