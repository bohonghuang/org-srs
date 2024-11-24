;; -*- lexical-binding: t; -*-

(require 'parse-time)

(cl-deftype org-srs-timestamp () 'string)

(defun org-srs-timestamp-now (&optional time)
  (format-time-string "%FT%TZ" time "UTC0"))

(defun org-srs-timestamp-difference (time-a time-b)
  (- (time-to-seconds (parse-iso8601-time-string time-a))
     (time-to-seconds (parse-iso8601-time-string time-b))))

(defun org-srs-timestamp+ (time amount unit)
  (org-srs-now
   (+ (time-to-seconds (parse-iso8601-time-string time))
      (* amount (cl-ecase unit (:sec 1) (:minute 60) (:hour 3600) (:day 86400))))))

(defconst org-srs-timestamp-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "T" (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) "Z"))

(provide 'org-srs-time)
