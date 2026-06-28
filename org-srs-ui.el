;;; org-srs-ui.el --- User interfaces of Org-srs -*- lexical-binding: t -*-

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

;; This package serves as the base package for user interfaces of Org-srs.

;;; Code:

(require 'cl-lib)
(require 'custom)

(require 'org-srs-property)

(defgroup org-srs-ui nil
  "User interfaces for Org-srs."
  :group 'org-srs
  :prefix "org-srs-ui-")

;;;###autoload
(define-minor-mode org-srs-ui-mode
  "Minor mode to enable all Org-srs user interfaces."
  :group 'org-srs-ui
  :global t
  (cl-loop with arg = (if org-srs-ui-mode +1 -1)
           for member in (org-srs-property-group-members 'org-srs-ui #'org-srs-property-minor-mode-p)
           unless (eq member 'org-srs-ui-mode)
           when (org-srs-property-minor-mode-p member)
           do (cl-assert (fboundp member)) (cl-assert (boundp member))
           (funcall member arg)))

(provide 'org-srs-ui)
;;; org-srs-ui.el ends here
