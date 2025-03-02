;;; org-srs-time.el --- Time(stamp) utilities -*- lexical-binding:t -*-

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

;; This package defines the timestamp representation for Org-srs and
;; provides various utility functions for time(stamp) operations.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'parse-time)

(require 'org-srs-property)

(defconst org-srs-time-units '((:sec . 1) (:minute . 60) (:hour . 3600) (:day . 86400)))

(defun org-srs-time-desc-seconds (desc)
  (cl-loop for (amount unit) on desc by #'cddr
           sum (* amount (alist-get unit org-srs-time-units))))

(defun org-srs-time+ (time &rest desc)
  (time-add time (seconds-to-time (org-srs-time-desc-seconds desc))))

(defun org-srs-time-difference (time-a time-b)
  (- (time-to-seconds time-a) (time-to-seconds time-b)))

(cl-defun org-srs-time-truncate-hms (time)
  (let* ((time (decode-time time))
         (hms (cl-subseq time 0 3)))
    (cl-values (encode-time (cl-fill time 0 :start 0 :end 3)) (cl-mapcan #'list hms '(:sec :minute :hour)))))

(org-srs-property-defcustom org-srs-time-start-of-next-day '(4 :hour)
  "The offset used to calculate the start time of the next day."
  :group 'org-srs
  :type 'sexp)

(defvar org-srs-time-now #'current-time)
(defsubst org-srs-time-now () (funcall org-srs-time-now))

(defun org-srs-time-today ()
  (cl-multiple-value-bind (time hms) (org-srs-time-truncate-hms (org-srs-time-now))
    (let ((start-of-day (org-srs-time-start-of-next-day)))
      (if (< (org-srs-time-desc-seconds hms) (org-srs-time-desc-seconds start-of-day))
          (apply #'org-srs-time+ time -1 :day start-of-day)
        (apply #'org-srs-time+ time start-of-day)))))

(defun org-srs-time-tomorrow ()
  (org-srs-time+ (org-srs-time-today) 1 :day))

(cl-defun org-srs-time-today-p (time)
  (let ((seconds (time-to-seconds time)))
    (and (<= (time-to-seconds (org-srs-time-today)) seconds)
         (< seconds (time-to-seconds (org-srs-time-tomorrow))))))

(cl-deftype org-srs-timestamp () 'string)

(defalias 'org-srs-timestamp-time 'parse-iso8601-time-string)

(cl-defun org-srs-timestamp-now (&optional (time (org-srs-time-now)))
  (format-time-string "%FT%TZ" time "UTC0"))

(defalias 'org-srs-timestamp 'org-srs-timestamp-now)

(defun org-srs-timestamp-difference (time-a time-b)
  (- (time-to-seconds (org-srs-timestamp-time time-a))
     (time-to-seconds (org-srs-timestamp-time time-b))))

(defun org-srs-timestamp+ (time &rest desc)
  (org-srs-timestamp-now
   (+ (time-to-seconds (org-srs-timestamp-time time))
      (org-srs-time-desc-seconds desc))))

(defun org-srs-timestamp-min (&rest args)
  (cl-reduce (lambda (time-a time-b) (if (cl-plusp (org-srs-timestamp-difference time-a time-b)) time-b time-a)) args))

(defconst org-srs-timestamp-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "T" (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) "Z"))

(provide 'org-srs-time)
;;; org-srs-time.el ends here
