;; -*- lexical-binding: t; -*-

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
