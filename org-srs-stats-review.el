;;; org-srs-stats-review.el --- Review statistics and visualization -*- lexical-binding:t -*-

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

(defgroup org-srs-stats-review nil
  "Review statistics and visualization."
  :group 'org-srs-stats
  :prefix "org-srs-stats-review-")

(defun org-srs-stats-review-history-full (source)
  (let ((table (make-hash-table :test #'equal)))
    (org-srs-query
     `(and (not (or suspended new))
           ,(lambda ()
              (org-srs-table-goto-starred-line)
              (cl-loop with columns = (org-srs-table-column-name-number-alist)
                       with marker = (point-marker)
                       initially (forward-line -1)
                       for line = (cl-acons 'marker marker (org-srs-table-current-line columns))
                       for timestamp = (alist-get 'timestamp line)
                       for date = (org-srs-timestamp-date timestamp)
                       while (alist-get 'rating line)
                       do (push line (gethash date table))
                       until (cl-minusp (forward-line -1))
                       until (org-at-table-hline-p))))
     source)
    table))

(cl-defun org-srs-stats-review-history (source &optional (length (truncate (window-width) (/ 10.0 3.0))))
  (cl-loop with table = (org-srs-stats-review-history-full source)
           with labels and values
           for timestamp = (org-srs-timestamp-now) then (org-srs-timestamp+ timestamp -1 :day)
           for date = (org-srs-timestamp-date timestamp)
           for reviews = (gethash date table)
           repeat length
           do (push (substring date 5) labels) (push reviews values)
           finally (cl-return (cl-values labels values))))

;;;###autoload
(defun org-srs-stats-reviews (source)
  "Display a chart of the number of reviews per date for the given SOURCE."
  (interactive (list (org-srs-review-source-dwim)))
  (cl-multiple-value-bind (labels values) (org-srs-stats-review-history source)
    (chart-bar-quickie 'vertical "Org-srs Statistics: Reviews" labels "Date" (mapcar #'length values) "Reviews")))

;;;###autoload
(defun org-srs-stats-review-items (source)
  "Display a chart of the count of review items per date for the given SOURCE."
  (interactive (list (org-srs-review-source-dwim)))
  (cl-multiple-value-bind (labels values) (org-srs-stats-review-history source)
    (chart-bar-quickie 'vertical "Org-srs Statistics: Review Items" labels "Date"
                       (cl-loop for reviews in values
                                collect (length (cl-delete-duplicates reviews :key (apply-partially #'alist-get 'marker))))
                       "Items")))

(provide 'org-srs-stats-review)
;;; org-srs-stats-review.el ends here
