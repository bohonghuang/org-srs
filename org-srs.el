;;; org-srs.el --- A flexible spaced repetition system for Org-mode  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Bohong Huang

;; Author: Bohong Huang <bohonghuang@qq.com>
;; Maintainer: Bohong Huang <bohonghuang@qq.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org "9.6") (fsrs "1.0"))
;; URL: https://github.com/bohonghuang/org-srs
;; Keywords: memory, flashcards

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

;; Org-srs is a feature-rich and flexible spaced repetition system,
;; integrated into your learning and knowledge management workflow in
;; Org-mode.

;;; Code:

(require 'org-srs-property)
(require 'org-srs-algorithm)
(require 'org-srs-time)
(require 'org-srs-table)
(require 'org-srs-log)
(require 'org-srs-item)
(require 'org-srs-query)
(require 'org-srs-review)
(require 'org-srs-step)

(require 'org-srs-algorithm-fsrs)
(setf org-srs-algorithm (or org-srs-algorithm 'fsrs))

(require 'org-srs-item-card)
(require 'org-srs-item-cloze)

(provide 'org-srs)
;;; org-srs.el ends here
