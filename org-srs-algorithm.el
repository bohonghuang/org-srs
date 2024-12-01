;;; org-srs-algorithm.el --- Interface for algorithms  -*- lexical-binding:t -*-

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

;; This package serves as the algorithm interface for Org-srs, allowing
;; the core to be independent of specific algorithms.

;;; Code:

(require 'cl-generic)

(require 'org-srs-property)

(org-srs-property-defcustom org-srs-algorithm nil
  "The spaced repetition algorithm used globally."
  :group 'org-srs
  :type 'sexp)

(cl-defgeneric org-srs-algorithm-ensure (object &rest _args) object)

(defun org-srs-algorithm-current ()
  (apply #'org-srs-algorithm-ensure (ensure-list (org-srs-algorithm))))

(cl-defgeneric org-srs-algorithm-repeat (algorithm args))

(provide 'org-srs-algorithm)
;;; org-srs-algorithm.el ends here
