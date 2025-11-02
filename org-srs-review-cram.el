;;; org-srs-review-cram.el --- Cram review items for upcoming exams -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Bohong Huang

;; Author: Bohong Huang <bohonghuang@qq.com>
;; Maintainer: Bohong Huang <bohonghuang@qq.com>
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (org "9.7") (fsrs "6.0"))
;; URL: https://github.com/bohonghuang/org-srs
;; Keywords: outlines

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

;; This package provides cramming support for Org-srs to review items
;; that are not yet due but have short intervals, allowing them to be
;; rated without disrupting their original review schedules. You can also
;; fully customize your own cramming strategy.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-algorithm)
(require 'org-srs-entry)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-review-strategy)
(require 'org-srs-review-rate)

(defgroup org-srs-review-cram nil
  "Cram review items and rate them without modifying review histories."
  :group 'org-srs-review
  :prefix "org-srs-review-cram-")

(cl-defstruct org-srs-review-cram-algorithm
  "A dummy algorithm structure for recording rating performance when cramming.

STRATEGY is the cram review strategy used.
RATINGS is a hash table mapping review items to their ratings."
  (strategy `(org-srs-review-cram ,(org-srs-review-cram-strategy)) :type list)
  (ratings (make-hash-table :test #'equal) :type hash-table))

(defvar org-srs-review-cram-algorithm nil
  "Variable used to temporarily store `org-srs-review-cram-algorithm' when rating.")

(defvar org-srs-review-item)

(cl-defmethod org-srs-algorithm-repeat :around (_ args &context (org-srs-review-cram-algorithm org-srs-review-cram-algorithm))
  "Extract the rating from ARGS and record it in `org-srs-review-cram-algorithm'."
  (let ((algorithm org-srs-review-cram-algorithm)
        (rating (alist-get 'rating args)))
    (cl-assert (eq (car (org-srs-review-cram-algorithm-strategy algorithm)) 'org-srs-review-cram))
    (setf (gethash org-srs-review-item (org-srs-review-cram-algorithm-ratings algorithm)) rating)
    nil))

(cl-defmethod org-srs-review-strategy-items ((_state (eql 'todo)) (_strategy (eql 'org-srs-review-cram)) &rest args)
  "Sort items to be reviewed matching the strategy in ARGS based on point."
  (cl-destructuring-bind (strategy algorithm &aux (ratings (org-srs-review-cram-algorithm-ratings algorithm))) args
    (cl-delete-if
     (lambda (item) (gethash item ratings))
     (cl-loop with item-current
              = (when (eq major-mode 'org-mode)
                  (save-excursion
                    (goto-char (org-srs-entry-beginning-position))
                    (ignore-errors
                      (re-search-forward org-srs-item-header-regexp (org-srs-entry-end-position))
                      (cl-multiple-value-list (org-srs-item-at-point)))))
              and items = (org-srs-review-strategy-items 'todo strategy)
              with dummy-cons = (cons nil items)
              initially (unless item-current (cl-return items))
              for cons = dummy-cons then (cdr cons)
              for (nil item) = cons
              while item
              when (org-srs-item-equal item item-current)
              return (nconc (cl-shiftf (cdr cons) nil) (cdr dummy-cons))
              finally (cl-return (cdr dummy-cons))))))

(org-srs-property-defcustom org-srs-review-cram-strategy `(sort (ahead old ,(org-srs-timestamp-time "2999-12-31T23:59:59Z")) interval)
  "Review strategy used in cram review sessions."
  :group 'org-srs-review-cram
  :type 'sexp)

(defvar org-srs-review-rating)

(defvar org-srs-review-cram-finish-hook nil
  "Hook called with the rated review items when finishing a cram review session.")

;;;###autoload
(defun org-srs-review-cram-start (&rest args)
  "Start a cram review session.

ARGS are passed to `org-srs-review-start' as is."
  (interactive)
  (let* ((algorithm (make-org-srs-review-cram-algorithm))
         (strategy (nconc (org-srs-review-cram-algorithm-strategy algorithm) (list algorithm))))
    (prog1 (org-srs-property-let ((org-srs-review-strategy strategy))
             (if (called-interactively-p 'any)
                 (call-interactively 'org-srs-review-ahead-start)
               (apply #'org-srs-review-start args)))
      (cl-labels ((clear-rating ()
                    (cl-assert (boundp 'org-srs-review-rating))
                    (setf org-srs-review-rating nil))
                  (setup ()
                    (setf org-srs-review-cram-algorithm algorithm))
                  (teardown ()
                    (setf org-srs-review-cram-algorithm nil))
                  (continue ()
                    (if (org-srs-reviewing-p)
                        (progn
                          (org-srs-item-add-hook-once 'org-srs-review-before-rate-hook #'clear-rating -90)
                          (org-srs-item-add-hook-once 'org-srs-review-before-rate-hook #'setup 10)
                          (org-srs-item-add-hook-once 'org-srs-review-after-rate-hook #'clear-rating -90)
                          (org-srs-item-add-hook-once 'org-srs-review-after-rate-hook #'teardown 10)
                          (org-srs-item-add-hook-once 'org-srs-review-continue-hook #'continue 60))
                      (setf (org-srs-review-cram-algorithm-strategy algorithm) nil)
                      (run-hook-with-args 'org-srs-review-cram-finish-hook (org-srs-review-cram-algorithm-ratings algorithm)))))
        (continue)))))

(defun org-srs-review-cram-message-retention-rate (items)
  "Display the review performance based on ITEMS."
  (cl-loop with ratings = (mapcar (lambda (rating) (cons rating 0)) org-srs-review-ratings)
           for rating being the hash-value of items
           do (cl-incf (alist-get rating ratings))
           finally
           (message
            "Retention rate: %d%% (%s)"
            (/ (* (cl-reduce #'+ (butlast ratings) :key #'cdr) 100) (max (cl-reduce #'+ ratings :key #'cdr) 1))
            (cl-reduce
             (lambda (a b) (concat a "/" b))
             (cl-loop for (rating . count) in ratings
                      collect (format "%s: %d" (capitalize (string-trim-left (symbol-name rating) ":")) count))))))

(add-hook 'org-srs-review-cram-finish-hook #'org-srs-review-cram-message-retention-rate)

(provide 'org-srs-review-cram)
;;; org-srs-review-cram.el ends here
