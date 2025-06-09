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
(require 'project)

(require 'org-srs-property)
(require 'org-srs-table)
(require 'org-srs-log)
(require 'org-srs-query)
(require 'org-srs-item)
(require 'org-srs-time)
(require 'org-srs-review-strategy)

(defgroup org-srs-review nil
  "Scheduling and reviewing items within specified scopes."
  :group 'org-srs
  :prefix "org-srs-review-")

(defvar org-srs-review-item nil)

(cl-eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst org-srs-review-ratings '(:easy :good :hard :again)))

(defvar org-srs-review-before-rate-hook nil)
(defvar org-srs-review-after-rate-hook nil)

(defvar org-srs-reviewing-p)

(defvar org-srs-reviewing-predicates (list (apply-partially #'local-variable-p 'org-srs-review-after-rate-hook)))

(defun org-srs-reviewing-p ()
  (if (boundp 'org-srs-reviewing-p) org-srs-reviewing-p
    (cl-loop for predicate in org-srs-reviewing-predicates thereis (funcall predicate))))

(defvar org-srs-review-source)

(defvar org-srs-review-rating)

(defalias 'org-srs-review-add-hook-once 'org-srs-item-add-hook-once)

(cl-defun org-srs-review-rate (rating &rest args &aux (item (or args org-srs-review-item)))
  (cl-assert (not buffer-read-only) nil "Buffer %S must be editable" (current-buffer))
  (if org-srs-review-item
      (prog2 (let ((org-srs-review-rating rating))
               (run-hooks 'org-srs-review-before-rate-hook))
          (org-srs-item-with-current item
            (org-srs-table-goto-starred-line)
            (unless args
              (cl-assert
               (time-less-p
                (org-srs-timestamp-time (org-srs-table-field 'timestamp))
                (org-srs-time-tomorrow))))
            (org-srs-item-repeat (cl-nth-value 0 (org-srs-item-at-point)) rating))
        (let ((org-srs-review-rating rating))
          (run-hooks 'org-srs-review-after-rate-hook)))
    (cl-assert args)
    (let ((org-srs-review-item args))
      (apply #'org-srs-review-rate rating args))))

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

(defconst org-srs-review-orders
  '((const :tag "Position" position)
    (const :tag "Random" random)
    (const :tag "Due date" due-date)
    (const :tag "Priority" priority)))

(org-srs-property-defcustom org-srs-review-order-new-review 'review-first
  "Relative display order between new items and review items."
  :group 'org-srs-review
  :type `(choice
          (const :tag "New-first (due)" new-first)
          (const :tag "New-first (due and undue)" new-ahead)
          (const :tag "Review-first (due)" review-first)
          (const :tag "Review-first (due and undue)" review-ahead)
          . ,org-srs-review-orders))

(org-srs-property-defcustom org-srs-review-order-new 'position
  "Display order of new items."
  :group 'org-srs-review
  :type `(choice . ,org-srs-review-orders))

(org-srs-property-defcustom org-srs-review-order-review 'due-date
  "Display order of review items."
  :group 'org-srs-review
  :type `(choice . ,org-srs-review-orders))

(cl-defun org-srs-review-due-items (&optional (source (current-buffer)))
  (cl-flet ((ahead (strategy)
              (if-let ((ahead-time (org-srs-review-learn-ahead-time)))
                  `(ahead ,strategy ,ahead-time)
                strategy))
            (limit-total-reviews (strategy)
              (if (org-srs-review-new-items-ignore-review-limit-p)
                  strategy
                `(or (limit ,strategy ,(org-srs-review-max-reviews-per-day)) reviewing))))
    (let ((org-srs-review-source source))
      (org-srs-review-strategy-items
       'todo
       (or (org-srs-review-strategy)
           (let ((strategy-new `(subseq
                                 (or (sort
                                      (intersection (done new) reviewing)
                                      ,(org-srs-review-order-review))
                                     (sort
                                      (limit new ,(if (org-srs-review-new-items-ignore-review-limit-p)
                                                      (org-srs-review-new-items-per-day)
                                                    (min (- (org-srs-review-max-reviews-per-day)
                                                            (length (org-srs-review-strategy-items 'todo 'old))
                                                            (length (org-srs-review-strategy-items 'done 'old)))
                                                         (org-srs-review-new-items-per-day))))
                                      ,(org-srs-review-order-new)))))
                 (strategy-review `(subseq
                                    (sort
                                     (union
                                      (intersection (done old) reviewing)
                                      ,(if (org-srs-review-new-items-ignore-review-limit-p)
                                           `(limit old ,(org-srs-review-max-reviews-per-day))
                                         'old))
                                     ,(org-srs-review-order-review)))))
             (let ((order (org-srs-review-order-new-review)))
               (cl-case order
                 (new-ahead (limit-total-reviews `(or ,(ahead strategy-new) ,(ahead strategy-review))))
                 (review-ahead (limit-total-reviews `(or ,(ahead strategy-review) ,(ahead strategy-new))))
                 (new-first (ahead (limit-total-reviews `(or ,strategy-new ,strategy-review))))
                 (review-first (ahead (limit-total-reviews `(or ,strategy-review ,strategy-new))))
                 (t (ahead (limit-total-reviews `(sort (union ,strategy-new ,strategy-review) ,order))))))))))))

(defun org-srs-review-sources ()
  (cl-delete
   nil
   (list
    (when (eq major-mode 'org-mode)
      (cons
       'region
       (when (region-active-p)
         (cons
          (copy-marker (region-beginning))
          (copy-marker (region-end))))))
    (when (eq major-mode 'org-mode)
      (cons 'buffer (cons (point-min-marker) (point-max-marker))))
    (when (eq major-mode 'org-mode)
      (when-let ((file (buffer-file-name)))
        (cons 'file file)))
    (cons 'directory default-directory)
    (when-let* ((project (project-current))
                (root (project-root project)))
      (cons 'project root)))
   :key #'cdr))

(org-srs-property-defcustom org-srs-review-learn-ahead-offset-time-p #'org-srs-time-today-p
  "Whether to offset the scheduled time by the time difference of learning ahead."
  :group 'org-srs-review
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "If scheduled for today" org-srs-time-today-p)))

(defvar org-srs-review-finish-hook nil)

(defun org-srs-review-continue-p ()
  (and (boundp 'org-srs-review-rating) (or (not (boundp 'org-srs-reviewing-p)) (symbol-value 'org-srs-reviewing-p))))

(defun org-srs-review-source-dwim ()
  (cl-destructuring-bind (&optional (arg 1) &aux (sources (org-srs-review-sources)))
      current-prefix-arg
    (cl-assert (not (org-srs-reviewing-p)))
    (if (> arg 1)
        (alist-get (read (completing-read "Review scope: " (mapcar #'car sources) nil t)) sources)
      (cdr (cl-first sources)))))

;;;###autoload
(cl-defun org-srs-review-start (&optional (source (cdr (cl-first (org-srs-review-sources)))))
  "Start a review session for items in SOURCE.

If called interactively with a `\\[universal-argument]` prefix or
ARG greater than 1, prompt the user to select the scope of items
to review."
  (interactive (list (org-srs-review-source-dwim)))
  (require 'org-srs)
  (if-let ((item-args (let ((org-srs-reviewing-p t)) (cl-first (org-srs-review-due-items source)))))
      (let ((item (cl-first item-args)) (org-srs-reviewing-p t))
        (apply #'org-srs-item-goto item-args)
        (cl-assert (not (local-variable-p 'org-srs-review-item)))
        (cl-assert (null org-srs-review-item))
        (setq-local org-srs-review-item item-args)
        (org-srs-review-add-hook-once
         'org-srs-review-after-rate-hook
         (lambda ()
           (cl-assert (local-variable-p 'org-srs-review-item))
           (cl-assert (consp org-srs-review-item))
           (kill-local-variable 'org-srs-review-item)
           (cl-assert (null org-srs-review-item)))
         99)
        (apply #'org-srs-item-review (car item) (cdr item))
        (org-srs-review-add-hook-once
         'org-srs-review-after-rate-hook
         (lambda () (when (org-srs-review-continue-p) (org-srs-review-start source)))
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
  (let ((org-srs-reviewing-p nil) (org-srs-review-rating nil))
    (run-hooks 'org-srs-review-before-rate-hook)
    (run-hooks 'org-srs-review-after-rate-hook)))

(provide 'org-srs-review)
;;; org-srs-review.el ends here
