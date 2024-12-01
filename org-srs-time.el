;;; org-srs-time.el --- Time(stamp) utilities -*- lexical-binding:t -*-

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

;; This package defines the timestamp representation for Org-srs and
;; provides various utility functions for time(stamp) operations.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'parse-time)

(cl-defun org-srs-time-truncate-hms (time &optional (d 0))
  (let ((time (cl-fill (decode-time time) 0 :start 0 :end 3)))
    (cl-incf (cl-fourth time) d)
    (encode-time time)))

(cl-defun org-srs-time-today (&optional (offset 0))
  (org-srs-time-truncate-hms (current-time) offset))

(defun org-srs-time-tomorrow ()
  (org-srs-time-today 1))

(cl-deftype org-srs-timestamp () 'string)

(defalias 'org-srs-timestamp-time 'parse-iso8601-time-string)

(defun org-srs-timestamp-now (&optional time)
  (format-time-string "%FT%TZ" time "UTC0"))

(defun org-srs-timestamp-difference (time-a time-b)
  (- (time-to-seconds (org-srs-timestamp-time time-a))
     (time-to-seconds (org-srs-timestamp-time time-b))))

(defun org-srs-timestamp+ (time amount unit)
  (org-srs-timestamp-now
   (+ (time-to-seconds (org-srs-timestamp-time time))
      (* amount (cl-ecase unit (:sec 1) (:minute 60) (:hour 3600) (:day 86400))))))

(defconst org-srs-timestamp-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "T" (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) "Z"))

(provide 'org-srs-time)
;;; org-srs-time.el ends here
