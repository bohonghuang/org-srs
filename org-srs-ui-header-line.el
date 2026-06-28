;;; org-srs-ui-header-line.el --- Review statistics on header lines -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Bohong Huang

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

;; This package provides support for displaying review session statistics
;; in the header line to allow real-time monitoring of your review
;; progress and performance.

;;; Code:

(require 'cl-lib)

(require 'org-srs-review)

(defgroup org-srs-ui-header-line nil
  "Display review statistics in the header line during reviews."
  :group 'org-srs-ui
  :prefix "org-srs-ui-header-line-")

;;;###autoload
(define-minor-mode org-srs-ui-header-line-mode
  "Minor mode to display review statistics in the header line during reviews."
  :group 'org-srs-ui-header-line
  :global t)

(defun org-srs-ui-header-line-statistics ()
  "Compute review statistics and return them as multiple values."
  (cl-loop with finished = (org-srs-review-finished-items)
           with upcoming = (org-srs-review-upcoming-items)
           with reviewing = (cl-intersection finished upcoming :test #'equal)
           with finished = (cl-nset-difference finished reviewing :test #'equal)
           with upcoming = (cl-nset-difference upcoming reviewing :test #'equal)
           for item in upcoming
           if (apply #'org-srs-query-item-p '(not new) item) collect item into due
           else collect item into new
           finally (cl-return (cl-values new due reviewing finished))))

(defun org-srs-ui-header-line-before-review (&rest _)
  "Show the header line with review statistics before reviewing a item."
  (cl-assert (org-srs-reviewing-p))
  (when org-srs-ui-header-line-mode
    (save-excursion
      (let* ((header-line-new (cl-loop for items in (cl-multiple-value-list (org-srs-ui-header-line-statistics))
                                       for face in '(homoglyph warning error success)
                                       concat (propertize (format " %d" (length items)) 'face face)))
             (header-line-old (cl-shiftf header-line-format header-line-new)))
        (org-srs-review-add-hook-once
         'org-srs-review-continue-hook
         (lambda ()
           (cl-assert (eq header-line-format header-line-new))
           (setf header-line-format header-line-old)))))))

(add-hook 'org-srs-item-before-review-hook #'org-srs-ui-header-line-before-review)

(provide 'org-srs-ui-header-line)
;;; org-srs-ui-header-line.el ends here
