;;; org-srs-review-ahead.el --- Review items ahead of schedule -*- lexical-binding: t -*-

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

;; This package enables preemptive review of items due in the upcoming
;; days after completing daily reviews, helping you utilize spare time to
;; alleviate future review pressure.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'custom)

(require 'org-srs-query)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-review-strategy)
(require 'org-srs-schedule-fuzz)

(defgroup org-srs-review-ahead nil
  "Review items due in the near future."
  :group 'org-srs-review
  :prefix "org-srs-review-ahead-")

(defvar org-srs-review-source)

(cl-defmethod org-srs-review-strategy-items ((_state (eql 'todo)) (_strategy (eql 'org-srs-review-ahead)) &rest args)
  "Filter old review items using predicates ARGS for `org-srs-review-ahead-start'."
  (org-srs-query `(and (not new) (not suspended) . ,args) org-srs-review-source))

;;;###autoload
(defun org-srs-review-ahead-start (&rest args)
  "Start a review session for items due in the near future.

ARGS are passed to `org-srs-review-start' as is."
  (interactive)
  (let ((today (org-srs-time-today)))
    (cl-flet* ((seconds-days (seconds)
                 (truncate seconds (* 60 60 24)))
               (item-due-days (&optional item)
                 (seconds-days (org-srs-time-difference (apply #'org-srs-item-due-time item) today)))
               (item< (a b &aux (days-a (item-due-days a)) (days-b (item-due-days b)))
                 (if (= days-a days-b)
                     (> (apply #'org-srs-item-interval a) (apply #'org-srs-item-interval b))
                   (< days-a days-b)))
               (itemp ()
                 (save-excursion
                   (<= (item-due-days) (seconds-days (org-srs-schedule-fuzz-interval-base (org-srs-item-interval)))))))
      (org-srs-property-let ((org-srs-review-strategy `(or reviewing (sort (difference (org-srs-review-ahead ,#'itemp) (done reviewing)) ,#'item<))))
        (if (called-interactively-p 'any)
            (call-interactively 'org-srs-review-ahead-start)
          (apply #'org-srs-review-start args))))))

(provide 'org-srs-review-ahead)
;;; org-srs-review-ahead.el ends here
