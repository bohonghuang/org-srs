;;; org-srs-schedule-offset.el --- Due date offsetting mechanism -*- lexical-binding:t -*-

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

;; This package adjusts scheduled timestamps when reviews occur outside
;; planned timeframes. It ensures learning-ahead items are scheduled
;; based on their original planned timestamps, preventing them from being
;; prioritized over due items that should be reviewed first.

;;; Code:

(require 'cl-lib)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-review)
(require 'org-srs-table)
(require 'org-srs-time)
(require 'org-srs-schedule)

(defgroup org-srs-schedule-offset nil
  "Offsetting due timestamps to handle undue reviews, such as learning ahead or overdue reviews."
  :group 'org-srs-schedule
  :prefix "org-srs-schedule-offset-")

(org-srs-property-defcustom org-srs-offset-learn-ahead-time-p #'org-srs-time-today-p
  "Whether to offset the scheduled time by the time difference of learning ahead."
  :group 'org-srs-schedule-offset
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "If scheduled for today" org-srs-time-today-p)))

(defun org-srs-schedule-offset-learn-ahead-due-timestamp (timestamp-due)
  (save-excursion
    (let ((timestamp-scheduled (org-srs-table-field 'timestamp))
          (timestamp-review (progn (forward-line -1) (org-srs-table-field 'timestamp))))
      (or (let* ((difference (max (org-srs-timestamp-difference timestamp-due timestamp-review) 0))
                 (timestamp-scheduled (org-srs-timestamp+ timestamp-scheduled difference :sec)))
            (when (let ((offset-time-p (org-srs-offset-learn-ahead-time-p)))
                    (cl-etypecase offset-time-p
                      (function (funcall offset-time-p (org-srs-timestamp-time timestamp-scheduled)))
                      (boolean offset-time-p)))
              timestamp-scheduled))
          timestamp-scheduled))))

(defun org-srs-schedule-offset-update-due-timestamp (timestamp-due)
  (if (boundp 'org-srs-review-rating)
      (when (symbol-value 'org-srs-review-rating)
        (goto-char org-srs-review-item-marker)
        (org-srs-table-goto-starred-line)
        (org-srs-property-let (org-srs-offset-learn-ahead-time-p)
          (org-srs-table-with-temp-buffer
            (setf (org-srs-table-field 'timestamp) (org-srs-schedule-offset-learn-ahead-due-timestamp timestamp-due)))))
    (setf (org-srs-table-field 'timestamp) (org-srs-schedule-offset-learn-ahead-due-timestamp timestamp-due))))

(defun org-srs-schedule-offset-before-rate-hook ()
  (when (bound-and-true-p org-srs-review-rating)
    (goto-char org-srs-review-item-marker)
    (org-srs-table-goto-starred-line)
    (org-srs-review-add-hook-once
     'org-srs-review-after-rate-hook
     (apply-partially #'org-srs-schedule-offset-update-due-timestamp (org-srs-table-field 'timestamp))
     80)))

(add-hook 'org-srs-review-before-rate-hook #'org-srs-schedule-offset-before-rate-hook)

(provide 'org-srs-schedule-offset)
;;; org-srs-schedule-offset.el ends here
