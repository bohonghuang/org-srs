;;; org-srs-review-cache.el --- Cache due items in a review session -*- lexical-binding:t -*-

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

;; This package provides a caching mechanism for due items in a review
;; session, speeding up retrieving the next review item.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)

(require 'org)
(require 'org-element)

(require 'org-srs-time)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-property)

(cl-defstruct org-srs-review-cache
  (items nil)
  (pending-items nil)
  (source nil)
  (learn-ahead-limit nil))

(defvar org-srs-review-cache nil)
(defsubst org-srs-review-cache () org-srs-review-cache)
(defsubst \(setf\ org-srs-review-cache\) (value) (setf org-srs-review-cache value))

(cl-pushnew #'org-srs-review-cache org-srs-reviewing-predicates)

(defun org-srs-review-cached-items (&optional source)
  (when-let ((cache (org-srs-review-cache)))
    (cl-assert (org-srs-review-cache-source cache))
    (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
    (setf (org-srs-review-cache-pending-items cache)
          (cl-loop for time-item in (org-srs-review-cache-pending-items cache)
                   for (time . item) = time-item
                   if (cl-plusp (org-srs-time-difference time (org-srs-time-now)))
                   collect time-item
                   else
                   do (push item (org-srs-review-cache-items cache))))
    (org-srs-review-cache-items cache)))

(defun \(setf\ org-srs-review-cached-items\) (value &optional source)
  (if value
      (let ((cache (or (org-srs-review-cache)
                       (setf org-srs-review-cache
                             (make-org-srs-review-cache
                              :source source :items value
                              :learn-ahead-limit (when-let ((item (cl-first value)))
                                                   (when (cl-plusp
                                                          (org-srs-timestamp-difference
                                                           (apply #'org-srs-item-due-timestamp item)
                                                           (org-srs-timestamp-now)))
                                                     (org-srs-review-learn-ahead-limit))))))))
        (cl-assert (org-srs-review-cache-source cache))
        (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
        (setf (org-srs-review-cache-items cache) value))
    (setf (org-srs-review-cache) nil)))

(org-srs-property-defcustom org-srs-review-cache-due-items-p nil
  "Non-nil means to cache due items in a review session.

This can increase the speed of retrieving the next review item
from a large set of review items, but it may reduce scheduling
accuracy."
  :group 'org-srs
  :type 'boolean)

(cl-defmethod org-srs-review-due-items :around (source)
  (if (org-srs-review-cache-due-items-p)
      (or (org-srs-review-cached-items source) (setf (org-srs-review-cached-items source) (cl-call-next-method)))
    (cl-call-next-method)))

(defun org-srs-review-cache-after-rate ()
  (defvar org-srs-review-rating)
  (if (and org-srs-review-rating (org-srs-review-cache-due-items-p))
      (with-current-buffer (marker-buffer org-srs-review-item-marker)
        (save-excursion
          (goto-char org-srs-review-item-marker)
          (goto-char (org-table-begin))
          (save-restriction
            (let* ((cache (org-srs-review-cache))
                   (element (org-element-at-point))
                   (due (org-srs-timestamp-time (org-srs-item-due-timestamp)))
                   (learn-ahead-limit (org-srs-review-cache-learn-ahead-limit cache)))
              (narrow-to-region (org-element-begin element) (org-element-end element))
              (let ((item (nconc (cl-multiple-value-list (org-srs-item-at-point)) (list (current-buffer))))
                    (items (org-srs-property-let ((org-srs-review-learn-ahead-limit learn-ahead-limit)) (org-srs-review-due-items-1))))
                (cl-flet ((item-equal (a b) (cl-loop for elem-a in a for elem-b in b repeat 2 always (equal elem-a elem-b))))
                  (unless (cl-find item items :test #'item-equal)
                    (setf (org-srs-review-cached-items) (cl-delete item (org-srs-review-cached-items) :test #'item-equal))
                    (when (cl-plusp (org-srs-time-difference (org-srs-time-tomorrow) due))
                      (push (cons due item) (org-srs-review-cache-pending-items cache))))))))))
    (setf (org-srs-review-cache) nil)))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-review-cache-after-rate 95)

(provide 'org-srs-review-cache)
;;; org-srs-review-cache.el ends here
