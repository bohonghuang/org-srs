;;; org-srs-fuzz.el --- Fuzzing mechanism for due dates -*- lexical-binding:t -*-

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

;; This package is used to fuzz the due dates of review items to prevent
;; multiple items from expiring on the same day, thereby reducing
;; cognitive load and enhancing long-term retention of information.

;;; Code:

(require 'cl-lib)
(cl-eval-when (:compile-toplevel :load-toplevel :execute) (cl-float-limits))

(require 'org-srs-property)
(require 'org-srs-review)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-time)

(org-srs-property-defcustom org-srs-fuzz-ranges '(((2.5 :day) . 0.15)
                                                  ((7.0 :day) . 0.1)
                                                  ((20.0 :day) . 0.05))
  "Fuzzing factors corresponding to different interval ranges."
  :group 'org-srs
  :type 'sexp)

(org-srs-property-defcustom org-srs-fuzz-unit '(1 :day)
  "The smallest unit of time change when fuzzing due timestamps."
  :group 'org-srs
  :type 'sexp)

(cl-defun org-srs-fuzz-interval-default (interval &optional (unit (org-srs-fuzz-unit)))
  (let ((interval-seconds (org-srs-time-desc-seconds interval))
        (unit-seconds (org-srs-time-desc-seconds unit)))
    (list (if (cl-plusp interval-seconds) (+ interval-seconds unit-seconds) 0.0) :sec)))

(org-srs-property-defcustom org-srs-fuzz-interval #'org-srs-fuzz-interval-default
  "Function used to calculate the interval for fuzzing from the base interval."
  :group 'org-srs
  :type 'function)

(cl-defun org-srs-fuzz-interval-round (interval &optional (unit (org-srs-fuzz-unit)))
  (let ((interval-seconds (org-srs-time-desc-seconds interval))
        (unit-seconds (org-srs-time-desc-seconds unit)))
    (list (* (round interval-seconds unit-seconds) unit-seconds) :sec)))

(cl-defun org-srs-fuzz-calculate-interval (time-scheduled time-review)
  (cl-flet ((cl-clamp (number min max) (if (< number min) min (if (> number max) max number))))
    (let ((interval (org-srs-time-desc-seconds
                     (funcall (org-srs-fuzz-interval)
                              (list (cl-loop with scheduled-interval-seconds = (org-srs-time-difference time-scheduled time-review)
                                             for ((interval . factor) (next-interval . nil)) on (org-srs-fuzz-ranges)
                                             for interval-seconds = (org-srs-time-desc-seconds interval)
                                             for next-interval-seconds = (if next-interval (org-srs-time-desc-seconds next-interval) cl-most-positive-float)
                                             sum (* factor (- (cl-clamp scheduled-interval-seconds interval-seconds next-interval-seconds) interval-seconds)))
                                    :sec)))))
      (list (if (cl-plusp interval) (- (cl-random (* 2.0 interval)) interval) 0.0) :sec))))

(defun org-srs-fuzz-due-timestamp ()
  (save-excursion
    (let* ((timestamp-scheduled (org-srs-table-field 'timestamp))
           (time-scheduled (org-srs-timestamp-time timestamp-scheduled)))
      (forward-line -1)
      (apply #'org-srs-timestamp+ timestamp-scheduled
             (org-srs-fuzz-interval-round (org-srs-fuzz-calculate-interval time-scheduled (org-srs-timestamp-time (org-srs-table-field 'timestamp))))))))

(defun org-srs-fuzz-update-due-timestamp ()
  (if (boundp 'org-srs-review-rating)
      (when (symbol-value 'org-srs-review-rating)
        (save-excursion
          (goto-char org-srs-review-item-marker)
          (org-srs-property-let (org-srs-fuzz-ranges org-srs-fuzz-unit)
            (org-srs-table-with-temp-buffer
              (org-srs-table-goto-starred-line)
              (setf (org-srs-table-field 'timestamp) (org-srs-fuzz-due-timestamp))))
          (org-srs-log-hide-drawer)))
    (setf (org-srs-table-field 'timestamp) (org-srs-fuzz-due-timestamp))))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-fuzz-update-due-timestamp 60)

(provide 'org-srs-fuzz)
;;; org-srs-fuzz.el ends here
