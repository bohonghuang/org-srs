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
  "Cramming review items and rating them without modifying review histories."
  :group 'org-srs-review
  :prefix "org-srs-review-cram-")

(cl-defstruct org-srs-review-cram-algorithm
  "A dummy algorithm structure for cramming.

STRATEGY is the cram review strategy used."
  (strategy `(org-srs-review-cram ,(org-srs-review-cram-strategy)) :type list))

(defvar org-srs-review-cram-algorithm nil
  "Variable used to temporarily store `org-srs-review-cram-algorithm' when rating.")

(cl-defmethod org-srs-algorithm-repeat :around (_ args &context (org-srs-review-cram-algorithm org-srs-review-cram-algorithm))
  "Extract the rating from ARGS and record it in `org-srs-review-cram-algorithm'."
  (let ((algorithm org-srs-review-cram-algorithm))
    (cl-assert (eq (car (org-srs-review-cram-algorithm-strategy algorithm)) 'org-srs-review-cram))
    nil))

(cl-defmethod org-srs-review-strategy-items ((_state (eql 'todo)) (_strategy (eql 'org-srs-review-cram)) &rest args)
  "Sort items to be reviewed matching the strategy in ARGS based on point."
  (cl-destructuring-bind (strategy _algorithm) args
    (cl-loop with item-current
             = (or (when (eq major-mode 'org-mode)
                     (save-excursion
                       (goto-char (org-srs-entry-beginning-position))
                       (ignore-errors
                         (re-search-forward org-srs-item-header-regexp (org-srs-entry-end-position))
                         (cl-multiple-value-list (org-srs-item-at-point)))))
                   t)
             and items = (org-srs-review-strategy-items 'todo strategy)
             for item-cons on items
             for (item . rest) = item-cons
             when (equal (setf (car item-cons) (cl-subseq item 0 2)) item-current)
             do (setf (cdr item-cons) nil) and return (nconc rest items)
             finally (cl-return items))))

(org-srs-property-defcustom org-srs-review-cram-strategy `(sort (ahead old ,(org-srs-timestamp-time "2999-12-31T23:59:59Z")) interval)
  "Review strategy used in cram review sessions."
  :group 'org-srs-review-cram
  :type 'sexp)

(defvar org-srs-review-rating)

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
                      (setf (org-srs-review-cram-algorithm-strategy algorithm) nil))))
        (continue)))))

(provide 'org-srs-review-cram)
;;; org-srs-review-cram.el ends here
