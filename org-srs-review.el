;;; org-srs-review.el --- Learn and review due items -*- lexical-binding:t -*-

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

;; This package is used to scan, schedule, learn, and review due Org-srs
;; items, with various adjustable parameters to meet custom review needs.

;;; Code:

(require 'cl-lib)

(require 'org-srs-property)
(require 'org-srs-log)
(require 'org-srs-query)
(require 'org-srs-item)
(require 'org-srs-time)

(defvar org-srs-review-item-marker nil)

(cl-eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst org-srs-review-ratings '(:easy :good :hard :again)))

(defvar org-srs-review-after-rate-hook nil)

(defun org-srs-review-rate (rating &optional position)
  (save-excursion
    (if-let ((position (or position org-srs-review-item-marker)))
        (goto-char position)
      (cl-multiple-value-call #'org-srs-item-goto (org-srs-item-at-point)))
    (org-srs-table-goto-starred-line)
    (cl-assert
     (time-less-p
      (org-srs-timestamp-time (org-srs-table-field 'timestamp))
      (org-srs-time-tomorrow)))
    (org-srs-item-repeat (cl-nth-value 0 (org-srs-item-at-point)) rating)
    (org-srs-log-hide-drawer)
    (setf org-srs-review-item-marker nil))
  (run-hooks 'org-srs-review-after-rate-hook))

(defmacro org-srs-review-define-rating-commands ()
  `(progn . ,(cl-loop for rating in org-srs-review-ratings
                      for rating-name = (string-trim (symbol-name rating) ":")
                      collect `(defun ,(intern (format "%s%s" 'org-srs-review-rate- rating-name)) ()
                                 ,(format "Rate the item being reviewed as %s." rating-name)
                                 (interactive)
                                 (require 'org-srs)
                                 (org-srs-review-rate ,rating)))))

;;;###autoload (autoload 'org-srs-review-rate-easy "org-srs-review" "Rate the item being reviewed as easy." t)
;;;###autoload (autoload 'org-srs-review-rate-good "org-srs-review" "Rate the item being reviewed as good." t)
;;;###autoload (autoload 'org-srs-review-rate-hard "org-srs-review" "Rate the item being reviewed as hard." t)
;;;###autoload (autoload 'org-srs-review-rate-again "org-srs-review" "Rate the item being reviewed as again." t)
(org-srs-review-define-rating-commands)

(org-srs-property-defcustom org-srs-review-new-items-per-day 20
  "The maximum number of new item to introduce in a day."
  :group 'org-srs
  :type 'natnum)

(org-srs-property-defcustom org-srs-review-max-reviews-per-day 200
  "The maximum number of review items to show in a day."
  :group 'org-srs
  :type 'natnum)

(org-srs-property-defcustom org-srs-review-new-items-ignore-review-limit-p nil
  "Non-nil means new items will be shown regardless of the review limit."
  :group 'org-srs
  :type 'boolean)

(cl-defun org-srs-review-pending-items (&optional (source (current-buffer)))
  (cl-etypecase source
    (buffer
     (with-current-buffer source
       (let ((items-learned (org-srs-query-buffer (org-srs-query-predicate-learned)))
             (items-to-review (org-srs-query-predicate-and
                               (org-srs-query-predicate-due)
                               (org-srs-query-predicate-not (org-srs-query-predicate-reviewed))
                               (org-srs-query-predicate-not (org-srs-query-predicate-new))))
             (items-reviewed (org-srs-query-buffer (org-srs-query-predicate-reviewed)))
             (predicate-null (org-srs-query-predicate-or))
             (predicate-due-new (org-srs-query-predicate-and
                                 (org-srs-query-predicate-due)
                                 (org-srs-query-predicate-new)))
             (predicate-due-nonnew (org-srs-query-predicate-and
                                    (org-srs-query-predicate-due)
                                    (org-srs-query-predicate-not
                                     (org-srs-query-predicate-new))))
             (predicate-due-today (org-srs-query-predicate-due))
             (predicate-reviewed-due-today (org-srs-query-predicate-and
                                            (org-srs-query-predicate-reviewed)
                                            (org-srs-query-predicate-due (org-srs-time-tomorrow)))))
         (or
          (org-srs-query-buffer
           (if (< (length items-reviewed) (org-srs-review-max-reviews-per-day))
               (if (< (length items-learned) (org-srs-review-new-items-per-day))
                   (if (or (org-srs-review-new-items-ignore-review-limit-p)
                           (< (+ (length items-reviewed) (length items-to-review))
                              (org-srs-review-max-reviews-per-day)))
                       predicate-due-today
                     predicate-due-nonnew)
                 predicate-due-nonnew)
             (if (< (length items-learned) (org-srs-review-new-items-per-day))
                 (if (org-srs-review-new-items-ignore-review-limit-p)
                     predicate-due-new
                   predicate-null)
               predicate-null)))
          (org-srs-query-buffer predicate-reviewed-due-today)))))
    (string
     (cl-assert (file-exists-p source))
     (cl-assert (not (file-directory-p source)))
     (org-srs-review-pending-items (find-file-noselect source)))))

(defalias 'org-srs-review-add-hook-once 'org-srs-item-add-hook-once)

;;;###autoload
(defun org-srs-review-start (&rest args)
  "Start a review session with ARGS."
  (interactive)
  (require 'org-srs)
  (if-let ((item-and-id (cl-first (org-srs-review-pending-items))))
      (cl-destructuring-bind (item _id) item-and-id
        (apply #'org-srs-item-goto item-and-id)
        (setf org-srs-review-item-marker (point-marker))
        (org-srs-log-hide-drawer org-srs-review-item-marker)
        (apply #'org-srs-item-review (car item) (cdr item))
        (org-srs-log-hide-drawer org-srs-review-item-marker)
        (org-srs-review-add-hook-once
         'org-srs-review-after-rate-hook
         (apply #'apply-partially #'org-srs-review-start args)
         100))
    (message "Review done")))

;;;###autoload
(defun org-srs-review-quit ()
  "Quit the current review session."
  (interactive)
  (cl-assert (local-variable-p 'org-srs-review-after-rate-hook))
  (cl-assert (> (length org-srs-review-after-rate-hook) 1))
  (when-let ((position (cl-position t org-srs-review-after-rate-hook :from-end t :test-not #'eq)))
    (if (cl-plusp position)
        (pop (cdr (nthcdr (1- position) org-srs-review-after-rate-hook)))
      (pop org-srs-review-after-rate-hook)))
  (run-hooks 'org-srs-review-after-rate-hook)
  (kill-local-variable 'org-srs-review-after-rate-hook))

(provide 'org-srs-review)
;;; org-srs-review.el ends here
