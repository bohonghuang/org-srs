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

(defun org-srs-reviewing-p ()
  (local-variable-p 'org-srs-review-after-rate-hook))

(defvar org-srs-review-rating)

(cl-defun org-srs-review-rate (rating &optional (position org-srs-review-item-marker))
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
    (org-srs-item-run-hooks-once 'org-srs-review-after-rate-hook)))

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

(org-srs-property-defcustom org-srs-review-learn-ahead-limit #'org-srs-time-tomorrow
  "The maximum advance time for due items when no items are available for review."
  :group 'org-srs
  :type 'sexp)

(cl-defun org-srs-review-due-items-1 (&optional (query-function #'org-srs-query-buffer))
  (cl-macrolet ((query (predicate) `(funcall query-function ,predicate)))
    (let ((items-learned (query (org-srs-query-predicate-learned)))
          (items-to-review (org-srs-query-predicate-and
                            (org-srs-query-predicate-due)
                            (org-srs-query-predicate-not (org-srs-query-predicate-reviewed))
                            (org-srs-query-predicate-not (org-srs-query-predicate-new))))
          (items-reviewed (query (org-srs-query-predicate-reviewed))))
      (cl-flet ((predicate-pending (&optional (now (current-time)))
                  (let* ((predicate-null (org-srs-query-predicate-or))
                         (predicate-due-now (org-srs-query-predicate-due now))
                         (predicate-due-new (org-srs-query-predicate-and
                                             predicate-due-now
                                             (org-srs-query-predicate-new)))
                         (predicate-due-nonnew (org-srs-query-predicate-and
                                                predicate-due-now
                                                (org-srs-query-predicate-not
                                                 (org-srs-query-predicate-new)))))
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
                            predicate-null)
                        predicate-null)))))
        (or (query (predicate-pending))
            (query (predicate-pending
                    (let ((limit (org-srs-review-learn-ahead-limit)))
                      (cl-etypecase limit
                        (list
                         (apply #'org-srs-time+ (current-time) limit))
                        (function
                         (funcall limit)))))))))))

(cl-defun org-srs-review-due-items (&optional (source (current-buffer)))
  (org-srs-review-due-items-1 (org-srs-query-function source)))

(defalias 'org-srs-review-add-hook-once 'org-srs-item-add-hook-once)
(defalias 'org-srs-review-run-hooks-once 'org-srs-item-run-hooks-once)

(defconst org-srs-review-orders
  '((const :tag "Position" position)
    (const :tag "Random" random)
    (const :tag "Due date" due-date)))

(org-srs-property-defcustom org-srs-review-order-new-review 'review-first
  "The relative display order between new items and review items."
  :group 'org-srs
  :type `(choice
          (const :tag "New first" new-first)
          (const :tag "Review first" review-first)
          . ,org-srs-review-orders))

(org-srs-property-defcustom org-srs-review-order-new 'position
  "The display order of new items."
  :group 'org-srs
  :type `(choice . ,org-srs-review-orders))

(org-srs-property-defcustom org-srs-review-order-review 'due-date
  "The display order of review items."
  :group 'org-srs
  :type `(choice . ,org-srs-review-orders))

(cl-defun org-srs-review-next-due-item (&optional (source (current-buffer)))
  (save-excursion
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
                   with predicate-new = (org-srs-query-predicate-new)
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
  (if-let ((item-args (org-srs-review-next-due-item source)))
      (let ((item (cl-first item-args)))
        (apply #'org-srs-item-goto item-args)
        (cl-assert (not buffer-read-only) nil "Buffer must be editable.")
        (setf org-srs-review-item-marker (point-marker))
        (org-srs-log-hide-drawer org-srs-review-item-marker)
        (apply #'org-srs-item-review (car item) (cdr item))
        (org-srs-log-hide-drawer org-srs-review-item-marker)
        (org-srs-review-add-hook-once
         'org-srs-review-after-rate-hook
         (apply-partially #'org-srs-review-start source)
         100))
    (message "Review done")))

;;;###autoload
(defun org-srs-review-quit ()
  "Quit the current review session."
  (interactive)
  (cl-assert (org-srs-reviewing-p))
  (cl-assert (> (length org-srs-review-after-rate-hook) 1))
  (when-let ((position (cl-position t org-srs-review-after-rate-hook :from-end t :test-not #'eq)))
    (if (cl-plusp position)
        (pop (cdr (nthcdr (1- position) org-srs-review-after-rate-hook)))
      (pop org-srs-review-after-rate-hook)))
  (let ((org-srs-review-rating nil))
    (org-srs-review-run-hooks-once 'org-srs-review-after-rate-hook)))

(provide 'org-srs-review)
;;; org-srs-review.el ends here
