;;; org-srs-stats-history.el --- Review history statistics and visualization -*- lexical-binding:t -*-

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

;; This package provides statistical analysis and visualization for
;; review histories.

;;; Code:

(require 'cl-lib)
(require 'custom)
(require 'chart)

(require 'org-srs-time)
(require 'org-srs-stats)
(require 'org-srs-review)
(require 'org-srs-query)

(defgroup org-srs-stats-history nil
  "Review history statistics and visualization."
  :group 'org-srs-stats
  :prefix "org-srs-stats-history-")

(defun org-srs-stats-history-full (source)
  (let ((table (make-hash-table :test #'equal)))
    (org-srs-query
     `(and (not (or suspended new))
           ,(lambda ()
              (org-srs-table-goto-starred-line)
              (cl-loop with columns = (org-srs-table-column-name-number-alist)
                       with marker = (point-marker)
                       initially (forward-line -1)
                       for previous-line = (cons nil nil) then line
                       for line = (cl-acons 'marker marker (org-srs-table-current-line columns))
                       for timestamp = (alist-get 'timestamp line)
                       for date = (org-srs-timestamp-date timestamp)
                       while (alist-get 'rating line)
                       do (push (setf (cdr (last previous-line)) line) (gethash date table))
                       until (cl-minusp (forward-line -1))
                       until (org-at-table-hline-p))))
     source)
    table))

(cl-defun org-srs-stats-history (source &optional (length (truncate (- (window-width) (* 5 2)) 3)))
  (cl-loop with table = (org-srs-stats-history-full source)
           with labels and values
           for timestamp = (org-srs-timestamp-now) then (org-srs-timestamp+ timestamp -1 :day)
           for date = (org-srs-timestamp-date timestamp)
           for reviews = (gethash date table)
           repeat length
           do (push (substring date 5) labels) (push reviews values)
           finally (cl-return (cl-values labels values))))

;;;###autoload
(defun org-srs-stats-history-reviews (source)
  "Display a chart of the number of reviews per date for the given SOURCE."
  (interactive (list (org-srs-review-source-dwim)))
  (cl-multiple-value-bind (labels values) (org-srs-stats-history source)
    (chart-bar-quickie 'vertical "Org-srs Statistics: Reviews" labels "Date" (mapcar #'length values) "Reviews")))

;;;###autoload
(defun org-srs-stats-history-review-items (source)
  "Display a chart of the count of review items per date for the given SOURCE."
  (interactive (list (org-srs-review-source-dwim)))
  (cl-multiple-value-bind (labels values) (org-srs-stats-history source)
    (chart-bar-quickie 'vertical "Org-srs Statistics: Review Items" labels "Date"
                       (cl-loop for reviews in values
                                collect (length (cl-delete-duplicates reviews :key (apply-partially #'alist-get 'marker))))
                       "Items")))

;;;###autoload
(defun org-srs-stats-history-retentions (source)
  "Display a chart of the retention rate per date for the given SOURCE."
  (interactive (list (org-srs-review-source-dwim)))
  (cl-multiple-value-bind (labels values) (org-srs-stats-history source)
    (chart-bar-quickie 'vertical "Org-srs Statistics: Retentions" labels "Date"
                       (cl-loop for reviews in values
                                collect (cl-loop with table = (make-hash-table :test #'eq)
                                                 for review in reviews
                                                 for marker = (alist-get 'marker review)
                                                 for rating = (alist-get 'rating review)
                                                 when (cl-loop for (key . value) in review
                                                               count (eq key 'state) into count
                                                               when (= count 2)
                                                               return (eq value :review))
                                                 do (setf (gethash marker table t) (and (gethash marker table t) (not (eq rating :again))))
                                                 finally (cl-return
                                                          (condition-case nil
                                                              (cl-loop for successp being the hash-value in table
                                                                       count successp into success-count
                                                                       count (not successp) into failure-count
                                                                       finally (cl-return (/ (* success-count 100) (+ success-count failure-count))))
                                                            (arith-error 0)))))
                       "Retention")))

(provide 'org-srs-stats-history)
;;; org-srs-stats-history.el ends here
