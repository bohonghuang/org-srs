;;; org-srs-query.el --- Review item query facilities -*- lexical-binding:t -*-

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

;; This package provides operations for querying and filtering Org-srs
;; review items based on various composable predicates.

;;; Code:

(require 'cl-lib)
(require 'parse-time)

(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-item)
(require 'org-srs-time)

(defun org-srs-query-predicate-and (&rest predicates)
  (lambda () (cl-loop for predicate in predicates always (funcall predicate))))

(defun org-srs-query-predicate-or (&rest predicates)
  (lambda () (cl-loop for predicate in predicates thereis (funcall predicate))))

(defun org-srs-query-predicate-not (predicate)
  (lambda () (not (funcall predicate))))

(defun org-srs-query-predicate-new ()
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (forward-line -1)
        (or (org-at-table-hline-p) (string-empty-p (org-srs-table-field 'rating)))))))

(cl-defun org-srs-query-predicate-updated
    (&optional
     (from (org-srs-time-today) fromp)
     (to (unless fromp (org-srs-time-tomorrow))))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (forward-line -1)
        (unless (org-at-table-hline-p)
          (let ((time (org-srs-timestamp-time (org-srs-table-field 'timestamp))))
            (and (time-less-p from time) (or (null to) (time-less-p time to)))))))))

(cl-defun org-srs-query-predicate-due (&optional (now (current-time)))
  (lambda ()
    (save-excursion
      (when (re-search-forward org-srs-log-latest-timestamp-regexp (org-table-end))
        (time-less-p (org-srs-timestamp-time (match-string 2)) now)))))

(cl-defun org-srs-query-predicate-learned
    (&optional
     (from (org-srs-time-today) fromp)
     (to (unless fromp (org-srs-time-tomorrow))))
  (lambda ()
    (save-excursion
      (when-let ((time (cl-loop with end = (org-table-end)
                                initially (org-table-goto-line 2)
                                for previous-rating = "" then current-rating
                                for current-rating = (org-srs-table-field 'rating)
                                for current-timestamp = (org-srs-table-field 'timestamp)
                                when (and (string-empty-p previous-rating) (not (string-empty-p current-rating)))
                                return (org-srs-timestamp-time current-timestamp)
                                do (forward-line 1)
                                until (>= (point) end))))
        (and (time-less-p from time) (or (null to) (time-less-p time to)))))))

(defun org-srs-query-predicate-reviewed (&rest args)
  (org-srs-query-predicate-and
   (org-srs-query-predicate-not (org-srs-query-predicate-new))
   (apply #'org-srs-query-predicate-updated args)))

(cl-defun org-srs-query-region (predicate &optional (start (point-min)) (end (point-max)))
  (save-excursion
    (cl-loop initially (goto-char start)
             while (re-search-forward org-srs-item-header-regexp end t)
             when (save-match-data
                    (re-search-forward org-table-line-regexp)
                    (funcall predicate))
             collect (cl-multiple-value-list (org-srs-item-from-match-data)))))

(cl-defun org-srs-query-buffer (predicate &optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (org-srs-query-region predicate)))

(provide 'org-srs-query)
;;; org-srs-query.el ends here
