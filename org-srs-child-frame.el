;;; org-srs-child-frame.el --- Child frame utilities -*- lexical-binding:t -*-

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

;; This package offers convenient interfaces to create, display and
;; manage child frames.

;;; Code:

(require 'cl-lib)

(defvar org-srs-child-frames nil)

(cl-defun org-srs-child-frame-p (&optional (frame (selected-frame)))
  (cl-values-list (car (cl-rassoc frame org-srs-child-frames :test #'eq))))

(cl-defun org-srs-child-frame-root (&optional (frame (selected-frame)))
  (if-let ((parent (cl-nth-value 0 (org-srs-child-frame-p frame))))
      (progn
        (cl-assert (eq parent (frame-parent frame)))
        (cl-assert (null (frame-parent parent)))
        (org-srs-child-frame-root parent))
    frame))

(cl-defun org-srs-child-frames-1 (&optional (name nil namep))
  (if namep (cl-remove name org-srs-child-frames :key #'cadar :test-not #'eq) org-srs-child-frames))

(defun org-srs-child-frames (&rest args)
  (mapcar #'cdr (apply #'org-srs-child-frames-1 args)))

(defun \(setf\ org-srs-child-frames\) (value &rest args)
  (cl-assert (null value))
  (mapc #'delete-frame (apply #'org-srs-child-frames args))
  (setf org-srs-child-frames (cl-nset-difference org-srs-child-frames (apply #'org-srs-child-frames-1 args))))

(cl-defun org-srs-child-frame (name
                               &key
                               (parent (org-srs-child-frame-root))
                               (window (frame-selected-window parent))
                               (size (/ 16.0))
                               (position :bottom)
                               (buffer (get-buffer-create (format " *org-srs-child-frame %x/%s*" (sxhash-eq parent) name))))
  (cl-assert (null (frame-parent parent)))
  (cl-destructuring-bind (parent-left parent-top parent-right parent-bottom)
      (window-inside-absolute-pixel-edges window)
    (let ((parent-width (- parent-right parent-left))
          (parent-height (- parent-bottom parent-top)))
      (cl-multiple-value-bind (child-width child-height)
          (cl-etypecase size
            (float (cl-values parent-width (truncate (* parent-height size))))
            (cons (cl-values (car size) (cdr size))))
        (cl-multiple-value-bind (child-left child-top)
            (cl-etypecase position
              ((eql :bottom) (cl-values parent-left (- parent-bottom child-height)))
              (cons (cl-values (car position) (cdr position))))
          (cl-flet ((make-child-frame ()
                      (make-frame
                       `((name . "org-srs-child-frame")
                         (parent-frame . ,parent)
                         (no-accept-focus . t)
                         (visibility . nil)
                         (border-width . 0)
                         (minibuffer . nil)
                         (menu-bar-lines . 0)
                         (tool-bar-lines . 0)
                         (tab-bar-lines . 0)
                         (unsplittable . t)
                         (min-height . 0)
                         (min-width . 0)
                         (cursor-type . nil)
                         (no-other-frame . t)
                         (undecorated . t)
                         (no-special-glyphs . t)
                         (skip-taskbar . t)
                         (desktop-dont-save . t)))))
            (let ((frame (let ((key (list parent name)))
                           (or #1=(alist-get key org-srs-child-frames nil t #'equal)
                               (let ((frame (make-child-frame)))
                                 (if-let ((new-frame #1#))
                                     (progn (delete-frame frame) new-frame)
                                   (setf #1# frame)))))))
              (set-frame-size frame child-width child-height t)
              (set-frame-position frame child-left child-top)
              (set-window-buffer (frame-selected-window frame) buffer)
              (cl-assert (eq (window-buffer (frame-selected-window frame)) buffer))
              (setf (buffer-local-value 'mode-line-format buffer) nil
                    (buffer-local-value 'truncate-lines buffer) t
                    (buffer-local-value 'buffer-read-only buffer) t
                    (buffer-local-value 'cursor-type buffer) nil)
              frame)))))))

(provide 'org-srs-child-frame)
;;; org-srs-child-frame.el ends here
