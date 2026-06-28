;;; org-srs-review-undo.el --- Undo/redo facilities -*- lexical-binding: t -*-

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

;; This package provides undo and redo support for Org-srs review ratings by
;; recording snapshots of review item log drawers before ratings are applied.

;;; Code:

(require 'cl-lib)

(require 'org-srs-log)
(require 'org-srs-item)
(require 'org-srs-review)

(defgroup org-srs-review-undo nil
  "Undo and redo facilities for review ratings."
  :group 'org-srs-review
  :prefix "org-srs-review-undo-")

(cl-defstruct org-srs-review-undo-history-entry
  "Snapshot of a review item's log used for undo and redo.

MARKER points to the review item whose log was captured.
LOG stores the full contents of its Org-srs log drawer."
  (marker nil :type marker)
  (log "" :type string))

(defvar org-srs-review-source)

(cl-defstruct org-srs-review-undo-history
  "Undo and redo history for a review session.

UNDO is a stack of snapshots that can be restored by `org-srs-review-undo'.
REDO is a stack of snapshots that can be restored by `org-srs-review-undo-redo'.
SOURCE is the review source associated with this history."
  (undo nil :type list)
  (redo nil :type list)
  (source org-srs-review-source :type t))

(defvar org-srs-review-item)

(cl-defun org-srs-review-undo-snapshot (&optional (marker (apply #'org-srs-item-marker org-srs-review-item)))
  "Return a snapshot of the review log for the item at MARKER."
  (make-org-srs-review-undo-history-entry
   :marker marker
   :log (buffer-substring-no-properties
         (progn (org-srs-log-beginning-of-drawer) (point))
         (progn (org-srs-log-end-of-drawer) (point)))))

(defvar org-srs-review-undo-history)

(org-srs-property-defcustom org-srs-review-undo-history-length 10
  "Maximum number of undo snapshots kept for a review session."
  :group 'org-srs-review-undo
  :type 'integer)

(cl-defun org-srs-review-undo-before-rate (&optional (history org-srs-review-undo-history))
  "Save a snapshot to HISTORY before the current item is rated."
  (save-excursion
    (push (org-srs-review-undo-snapshot) (org-srs-review-undo-history-undo history))
    (when-let ((cons (nthcdr (1- (org-srs-review-undo-history-length)) (org-srs-review-undo-history-undo history))))
      (setf (cdr cons) nil))
    (setf (org-srs-review-undo-history-redo history) nil)))

(defun org-srs-review-undo-teardown ()
  "Clean up buffer-local undo history state for the current review item."
  (kill-local-variable 'org-srs-review-undo-history)
  (cl-assert (not (local-variable-p 'org-srs-review-undo-history))))

(cl-defun org-srs-review-undo-setup (&optional (history (or (bound-and-true-p org-srs-review-undo-history)
                                                            (make-org-srs-review-undo-history :source org-srs-review-source))))
  "Set up undo support for the current review session using HISTORY."
  (cl-assert (not (local-variable-p 'org-srs-review-undo-history)))
  (when (org-srs-reviewing-p)
    (setq-local org-srs-review-undo-history history)
    (org-srs-review-add-hook-once 'org-srs-review-before-rate-hook #'org-srs-review-undo-before-rate)
    (org-srs-review-add-hook-once 'org-srs-review-continue-hook #'org-srs-review-undo-teardown)
    (org-srs-review-add-hook-once 'org-srs-review-continue-hook (apply-partially #'org-srs-review-undo-setup history) 100)))

(add-hook 'org-srs-review-start-hook #'org-srs-review-undo-setup)

;;;###autoload
(cl-defun org-srs-review-undo (&optional (history org-srs-review-undo-history))
  "Undo the most recent rating in HISTORY for the current review session."
  (interactive)
  (cl-assert (org-srs-reviewing-p))
  (if-let ((entry (pop (org-srs-review-undo-history-undo history))))
      (let ((marker (org-srs-review-undo-history-entry-marker entry)))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (push (org-srs-review-undo-snapshot marker) (org-srs-review-undo-history-redo history))
           (delete-region
            (progn (org-srs-log-beginning-of-drawer) (point))
            (progn (org-srs-log-end-of-drawer) (point)))
           (insert (org-srs-review-undo-history-entry-log entry))
           (org-srs-log-hide-drawer)))
        (org-srs-review-quit)
        (let ((org-srs-review-undo-history history))
          (org-srs-review-start (org-srs-review-undo-history-source history))))
    (error "No more undo history")))

;;;###autoload
(cl-defun org-srs-review-undo-redo (&optional (history org-srs-review-undo-history))
  "Redo the most recently undone rating in HISTORY for the current review session."
  (interactive)
  (if-let ((entry (pop (org-srs-review-undo-history-redo history))))
      (let ((marker (org-srs-review-undo-history-entry-marker entry)))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (push (org-srs-review-undo-snapshot marker) (org-srs-review-undo-history-undo history))
           (delete-region
            (progn (org-srs-log-beginning-of-drawer) (point))
            (progn (org-srs-log-end-of-drawer) (point)))
           (insert (org-srs-review-undo-history-entry-log entry))
           (org-srs-log-hide-drawer)))
        (org-srs-review-quit)
        (let ((org-srs-review-undo-history history))
          (org-srs-review-start (org-srs-review-undo-history-source history))))
    (error "No more redo history")))

(provide 'org-srs-review-undo)
;;; org-srs-review-undo.el ends here
