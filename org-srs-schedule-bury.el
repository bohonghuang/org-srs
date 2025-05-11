;;; org-srs-schedule-bury.el --- Burying mechanism for review items -*- lexical-binding:t -*-

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

;; This package is used for burying due items, allowing some items that
;; should be due, such as sibling items, to be postponed for review,
;; thereby avoiding an impact on the accuracy of ratings.

;;; Code:

(require 'cl-lib)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-review)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-item)
(require 'org-srs-time)
(require 'org-srs-schedule)

(defgroup org-srs-schedule-bury nil
  "Burying mechanism to prevent relevant items from being shown on the same day."
  :group 'org-srs-schedule
  :prefix "org-srs-schedule-bury-")

(org-srs-property-defcustom org-srs-schedule-bury-sibling-items-p nil
  "Non-nil means to bury sibling items to postpone their reviews."
  :group 'org-srs-schedule-bury
  :type 'boolean)

(org-srs-property-defcustom org-srs-schedule-bury-interval '(1 :day)
  "Interval after which reviews of buried items are postponed."
  :group 'org-srs-schedule-bury
  :type 'sexp)

(cl-defun org-srs-schedule-bury-due-timestamp (&optional (interval (org-srs-schedule-bury-interval)))
  (let ((timestamp (org-srs-table-field 'timestamp)))
    (when (org-srs-timestamp< timestamp (org-srs-timestamp (org-srs-time-tomorrow)))
      (apply #'org-srs-timestamp+ (org-srs-timestamp-now) interval))))

(cl-defun org-srs-schedule-bury-update-due-timestamp (&optional (interval (org-srs-schedule-bury-interval)))
  (if (boundp 'org-srs-review-rating)
      (when (symbol-value 'org-srs-review-rating)
        (org-srs-item-with-current org-srs-review-item
          (when (org-srs-schedule-bury-sibling-items-p)
            (cl-loop with current-item = (cl-multiple-value-list (org-srs-item-at-point))
                     for item in (org-srs-query-region #'always (org-entry-beginning-position) (org-entry-end-position))
                     unless (equal item current-item)
                     do (org-srs-item-with-current item
                          (org-srs-table-goto-starred-line)
                          (when-let ((timestamp (org-srs-schedule-bury-due-timestamp interval)))
                            (setf (org-srs-item-due-timestamp) timestamp)))))))
    (when-let ((timestamp (org-srs-schedule-bury-due-timestamp interval)))
      (setf (org-srs-table-field 'timestamp) timestamp))))

(cl-defmethod org-srs-algorithm-repeat ((_ (eql 'org-srs-schedule-bury)) _) nil)

(cl-defun org-srs-schedule-bury (&optional (interval (org-srs-schedule-bury-interval)) (item org-srs-review-item))
  "Bury ITEM to postpone its review after INTERVAL.

If called interactively with a `\\[universal-argument]`, prompt the
user to specify the interval until the next review."
  (interactive (cl-destructuring-bind (&optional (arg 1)) current-prefix-arg
                 (cl-assert (org-srs-reviewing-p))
                 (when (> arg 1)
                   (list (read-minibuffer "Interval until the next review: " (format "%s" (org-srs-schedule-bury-interval)))))))
  (cl-loop (funcall (or (org-srs-item-confirm-pending-p) (cl-return))))
  (org-srs-item-with-current item
    (org-srs-table-goto-starred-line)
    (org-srs-schedule-bury-update-due-timestamp interval))
  (org-srs-property-let ((org-srs-algorithm 'org-srs-schedule-bury))
    (apply #'org-srs-review-rate nil item)))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-schedule-bury-update-due-timestamp 40)

(provide 'org-srs-schedule-bury)
;;; org-srs-schedule-bury.el ends here
