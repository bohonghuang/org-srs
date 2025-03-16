;;; org-srs-review.el --- Learn and review due items -*- lexical-binding:t -*-

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

;; This package is used to scan, schedule, learn, and review due Org-srs
;; items, with various adjustable parameters to meet custom review needs.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-table)
(require 'org-srs-log)
(require 'org-srs-query)
(require 'org-srs-item)
(require 'org-srs-time)

(defgroup org-srs-review nil
  "Scheduling and reviewing items within specified scopes."
  :group 'org-srs
  :prefix "org-srs-review-")

(defvar org-srs-review-item-marker nil)

(cl-eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst org-srs-review-ratings '(:easy :good :hard :again)))

(defvar org-srs-review-before-rate-hook nil)
(defvar org-srs-review-after-rate-hook nil)

(defvar org-srs-reviewing-p)

(defvar org-srs-reviewing-predicates (list (apply-partially #'local-variable-p 'org-srs-review-after-rate-hook)))

(defun org-srs-reviewing-p ()
  (or (bound-and-true-p org-srs-reviewing-p) (cl-loop for predicate in org-srs-reviewing-predicates thereis (funcall predicate))))

(defvar org-srs-review-rating)

(defalias 'org-srs-review-add-hook-once 'org-srs-item-add-hook-once)
(defalias 'org-srs-review-run-hooks-once 'org-srs-item-run-hooks-once)

(cl-defun org-srs-review-rate (rating &optional (position org-srs-review-item-marker))
  (let ((org-srs-review-rating rating))
    (org-srs-review-run-hooks-once 'org-srs-review-before-rate-hook))
  (save-excursion
    (if position (goto-char position) (cl-multiple-value-call #'org-srs-item-goto (org-srs-item-at-point)))
    (org-srs-table-goto-starred-line)
    (cl-assert
     (time-less-p
      (org-srs-timestamp-time (org-srs-table-field 'timestamp))
      (org-srs-time-tomorrow)))
    (org-srs-item-repeat (cl-nth-value 0 (org-srs-item-at-point)) rating)
    (org-srs-log-hide-drawer))
  (let ((org-srs-review-rating rating))
    (org-srs-review-run-hooks-once 'org-srs-review-after-rate-hook)))

(defmacro org-srs-review-define-rating-commands ()
  `(progn . ,(cl-loop for rating in org-srs-review-ratings
                      for rating-name = (string-trim (symbol-name rating) ":")
                      collect `(defun ,(intern (format "%s%s" 'org-srs-review-rate- rating-name)) ()
                                 ,(format "Rate the item being reviewed as %s." rating-name)
                                 (interactive)
                                 (require 'org-srs)
                                 (cl-assert (org-srs-reviewing-p))
                                 (org-srs-review-rate ,rating)))))

;;;###autoload (autoload 'org-srs-review-rate-easy "org-srs-review" "Rate the item being reviewed as easy." t)
;;;###autoload (autoload 'org-srs-review-rate-good "org-srs-review" "Rate the item being reviewed as good." t)
;;;###autoload (autoload 'org-srs-review-rate-hard "org-srs-review" "Rate the item being reviewed as hard." t)
;;;###autoload (autoload 'org-srs-review-rate-again "org-srs-review" "Rate the item being reviewed as again." t)
(org-srs-review-define-rating-commands)

(org-srs-property-defcustom org-srs-review-new-items-per-day 20
  "Maximum number of new item to introduce in a day."
  :group 'org-srs-review
  :type 'natnum)

(org-srs-property-defcustom org-srs-review-max-reviews-per-day 200
  "Maximum number of review items to show in a day."
  :group 'org-srs-review
  :type 'natnum)

(org-srs-property-defcustom org-srs-review-new-items-ignore-review-limit-p nil
  "Non-nil means new items will be shown regardless of the review limit."
  :group 'org-srs-review
  :type 'boolean)

(org-srs-property-defcustom org-srs-review-learn-ahead-limit #'org-srs-time-tomorrow
  "Maximum advance time for due items when no items are available for review."
  :group 'org-srs-review
  :type 'sexp)

(cl-defun org-srs-review-learn-ahead-time (&optional (now (org-srs-time-now)))
  (let ((limit (org-srs-review-learn-ahead-limit)))
    (cl-etypecase limit
      (list
       (apply #'org-srs-time+ now limit))
      (function
       (funcall limit)))))

(cl-defun org-srs-review-due-items (source &optional (learn-ahead-time (org-srs-review-learn-ahead-time)))
  (cl-flet ((org-srs-query (predicate &optional (source source))
              (org-srs-query predicate source)))
    (let ((items-learned (org-srs-query 'learned))
          (items-to-review (org-srs-query `(and (due ,learn-ahead-time) (not reviewed) (not new))))
          (items-reviewed (org-srs-query 'reviewed)))
      (cl-flet ((predicate-pending (&optional (now (org-srs-time-now) nowp))
                  (let* ((predicate-due-now (if nowp `(due ,now) '(due)))
                         (predicate-due-reviewed `(and ,predicate-due-now reviewed))
                         (predicate-due-new `(and ,predicate-due-now new))
                         (predicate-due-nonnew `(and ,predicate-due-now (not new)))
                         (predicate-due-now `(and ,predicate-due-now)))
                    (if (< (length items-reviewed) (org-srs-review-max-reviews-per-day))
                        (if (< (length items-learned) (org-srs-review-new-items-per-day))
                            (if (or (org-srs-review-new-items-ignore-review-limit-p)
                                    (< (+ (length items-reviewed) (length items-to-review))
                                       (org-srs-review-max-reviews-per-day)))
                                predicate-due-now
                              predicate-due-nonnew)
                          predicate-due-nonnew)
                      (if (< (length items-learned) (org-srs-review-new-items-per-day))
                          (if (org-srs-review-new-items-ignore-review-limit-p)
                              predicate-due-new
                            predicate-due-reviewed)
                        predicate-due-reviewed)))))
        (or (org-srs-query (predicate-pending))
            (org-srs-query (predicate-pending learn-ahead-time)))))))

(defconst org-srs-review-orders
  '((const :tag "Position" position)
    (const :tag "Random" random)
    (const :tag "Due date" due-date)))

(org-srs-property-defcustom org-srs-review-order-new-review 'review-first
  "Relative display order between new items and review items."
  :group 'org-srs-review
  :type `(choice
          (const :tag "New first" new-first)
          (const :tag "Review first" review-first)
          . ,org-srs-review-orders))

(org-srs-property-defcustom org-srs-review-order-new 'position
  "Display order of new items."
  :group 'org-srs-review
  :type `(choice . ,org-srs-review-orders))

(org-srs-property-defcustom org-srs-review-order-review 'due-date
  "Display order of review items."
  :group 'org-srs-review
  :type `(choice . ,org-srs-review-orders))

(cl-defun org-srs-review-next-due-item (&optional (source (current-buffer)))
  (save-window-excursion
    (cl-labels ((cl-random-elt (sequence)
                  (when sequence (elt sequence (random (length sequence)))))
                (cl-disjoin (&rest functions)
                  (lambda (&rest args)
                    (cl-loop for function in functions thereis (apply function args))))
                (timestamp-seconds (&optional (timestamp (org-srs-item-due-timestamp)))
                  (time-to-seconds (org-srs-timestamp-time timestamp)))
                (next-item (items order)
                  (cl-case order
                    (position (cl-first (cl-sort items #'< :key #'cl-first)))
                    (due-date (cl-first (cl-sort items #'< :key #'cl-second)))
                    (random (cl-random-elt items))
                    (t (cl-etypecase order (function (funcall order items)))))))
      (cl-multiple-value-bind (new-items review-items)
          (cl-loop with items = (org-srs-review-due-items source)
                   with predicate-new = (org-srs-query-predicate 'new)
                   for item in items
                   for index from 0
                   do (apply #'org-srs-item-goto item)
                   if (funcall predicate-new) collect (list index (timestamp-seconds) item) into new-items
                   else collect (list index (timestamp-seconds) item) into review-items
                   finally (cl-return (cl-values new-items review-items)))
        (let* ((new-item (next-item new-items (org-srs-review-order-new)))
               (review-item (next-item review-items (org-srs-review-order-review)))
               (items (cl-delete nil (list new-item review-item)))
               (order (org-srs-review-order-new-review))
               (order (cl-case order
                        (review-first (cl-disjoin #'cl-second #'cl-first))
                        (new-first #'cl-first)
                        (t order))))
          (cl-third (next-item items order)))))))

(defun org-srs-review-sources ()
  (cl-delete
   nil
   (list
    (cons
     'region
     (when (region-active-p)
       (cons
        (copy-marker (region-beginning))
        (copy-marker (region-end)))))
    (cons 'buffer (buffer-file-name))
    (cons 'directory default-directory))
   :key #'cdr))

(org-srs-property-defcustom org-srs-review-learn-ahead-offset-time-p #'org-srs-time-today-p
  "Whether to offset the scheduled time by the time difference of learning ahead."
  :group 'org-srs-review
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "If scheduled for today" org-srs-time-today-p)))

(defvar org-srs-review-finish-hook nil)

;;;###autoload
(cl-defun org-srs-review-start (&optional (source (cdr (cl-first (org-srs-review-sources)))))
  "Start a review session for items in SOURCE.

If called interactively with a `\\[universal-argument]` prefix or
ARG greater than 1, prompt the user to select the scope of items
to review."
  (interactive (list
                (cl-destructuring-bind (&optional (arg 1) &aux (sources (org-srs-review-sources)))
                    current-prefix-arg
                  (cl-assert (not (org-srs-reviewing-p)))
                  (if (> arg 1)
                      (alist-get (read (completing-read "Review scope: " (mapcar #'car sources) nil t)) sources)
                    (cdr (cl-first sources))))))
  (require 'org-srs)
  (if-let ((item-args (let ((org-srs-reviewing-p t)) (org-srs-review-next-due-item source))))
      (let ((item (cl-first item-args)) (org-srs-reviewing-p t))
        (apply #'org-srs-item-goto item-args)
        (when-let ((offset-time-p (org-srs-review-learn-ahead-offset-time-p)))
          (org-srs-review-add-hook-once
           'org-srs-review-after-rate-hook
           (let ((due-timestamp (org-srs-item-due-timestamp)))
             (lambda ()
               (when org-srs-review-rating
                 (let ((difference (org-srs-timestamp-difference due-timestamp (org-srs-timestamp-now))))
                   (when (cl-plusp difference)
                     (apply #'org-srs-item-goto item-args)
                     (org-srs-table-goto-starred-line)
                     (let ((due-timestamp (org-srs-timestamp+ (org-srs-table-field 'timestamp) difference :sec)))
                       (when (cl-etypecase offset-time-p
                               (function (funcall offset-time-p (org-srs-timestamp-time due-timestamp)))
                               ((eql t) t))
                         (setf (org-srs-table-field 'timestamp) due-timestamp))))))))
           75))
        (cl-assert (not buffer-read-only) nil "Buffer %S must be editable." (current-buffer))
        (cl-assert (not (local-variable-p 'org-srs-review-item-marker)))
        (cl-assert (null org-srs-review-item-marker))
        (setq-local org-srs-review-item-marker (point-marker))
        (org-srs-review-add-hook-once
         'org-srs-review-after-rate-hook
         (lambda ()
           (cl-assert (local-variable-p 'org-srs-review-item-marker))
           (cl-assert (markerp org-srs-review-item-marker))
           (kill-local-variable 'org-srs-review-item-marker)
           (cl-assert (null org-srs-review-item-marker)))
         99)
        (org-srs-log-hide-drawer org-srs-review-item-marker)
        (apply #'org-srs-item-review (car item) (cdr item))
        (org-srs-log-hide-drawer org-srs-review-item-marker)
        (org-srs-review-add-hook-once
         'org-srs-review-after-rate-hook
         (lambda () (when org-srs-review-rating (org-srs-review-start source)))
         100))
    (run-hook-with-args 'org-srs-review-finish-hook source)))

(defun org-srs-review-message-review-done (&rest _args)
  (message "Review done"))

(add-hook 'org-srs-review-finish-hook #'org-srs-review-message-review-done)

;;;###autoload
(defun org-srs-review-quit ()
  "Quit the current review session."
  (interactive)
  (cl-assert (org-srs-reviewing-p))
  (let ((org-srs-review-rating nil))
    (org-srs-review-run-hooks-once 'org-srs-review-before-rate-hook)
    (org-srs-review-run-hooks-once 'org-srs-review-after-rate-hook)))

(provide 'org-srs-review)
;;; org-srs-review.el ends here
