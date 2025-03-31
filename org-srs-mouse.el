;;; org-srs-mouse.el --- Mouse/touchscreen support for review interactions -*- lexical-binding:t -*-

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

;; This package provides mouse and touchscreen interaction support for
;; reviewing, allowing confirming and rating items through mouse events
;; (clicks/touches).

;;; Code:

(require 'cl-lib)

(require 'org-srs-child-frame)
(require 'org-srs-review)
(require 'org-srs-item)

(cl-defun org-srs-mouse-bottom-panel-hide (&optional (frame (org-srs-child-frame 'org-srs-mouse-bottom-panel)))
  (when (frame-visible-p frame)
    (make-frame-invisible frame)))

(cl-defun org-srs-mouse-string-pad-pixel (string &optional (width (string-pixel-width string)) (height (line-pixel-height)))
  (let ((space-width (/ (- width (string-pixel-width string)) 2)))
    (concat
     (propertize " " 'display `(space :width (,space-width) :height (,height)))
     string
     (propertize " " 'display `(space :width (,space-width) :height (,height))))))

(cl-defun org-srs-mouse-bottom-panel-show (labels
                                           &key
                                           (faces (make-list (length labels) 'default))
                                           (callback #'ignore))
  (let* ((child-frame (org-srs-child-frame 'org-srs-mouse-bottom-panel))
         (button-width (- (/ (frame-pixel-width child-frame) (float (length labels))) (* (string-pixel-width "​") 2)))
         (button-height (frame-pixel-height child-frame))
         (current-buffer (current-buffer)))
    (with-selected-frame (make-frame-visible child-frame)
      (cl-assert (not (eq current-buffer (current-buffer))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cl-mapc
         (lambda (label face)
           (insert "​")
           (insert-text-button
            (org-srs-mouse-string-pad-pixel (capitalize (string-trim-left (format "%s" label) ":")) button-width button-height)
            'face `((:foreground ,(face-foreground face))
                    (:inherit custom-button))
            'action (lambda (&optional _) (select-frame (frame-parent child-frame)) (funcall callback label)))
           (insert "​"))
         labels faces)
        (goto-char (point-min))))))

;;;###autoload
(define-minor-mode org-srs-mouse-mode
  "Minor mode to enable mouse/touchscreen input support for Org-srs review actions."
  :group 'org-srs-mouse
  :global t
  (if org-srs-mouse-mode
      (progn
        (add-hook 'window-selection-change-functions #'org-srs-mouse-mode-update-panels)
        (add-hook 'window-buffer-change-functions #'org-srs-mouse-mode-update-panels)
        (add-hook 'window-size-change-functions #'org-srs-mouse-mode-update-panels))
    (remove-hook 'window-selection-change-functions #'org-srs-mouse-mode-update-panels)
    (remove-hook 'window-buffer-change-functions #'org-srs-mouse-mode-update-panels)
    (remove-hook 'window-size-change-functions #'org-srs-mouse-mode-update-panels)
    (setf (org-srs-child-frames 'org-srs-mouse-bottom-panel) nil)))

(defun org-srs-mouse-mode-update-panels-1 ()
  (if (and org-srs-mouse-mode (eq major-mode 'org-mode) org-srs-review-item-marker)
      (if-let ((confirm-command (org-srs-item-confirm-pending-p)))
          (org-srs-mouse-bottom-panel-show
           '(continue)
           :callback (lambda (continue)
                       (cl-assert (eq continue 'continue))
                       (call-interactively confirm-command)))
        (org-srs-mouse-bottom-panel-show
         org-srs-review-ratings
         :faces '(homoglyph success warning error)
         :callback (lambda (rating)
                     (cl-assert (org-srs-reviewing-p))
                     (org-srs-review-rate rating))))
    (org-srs-mouse-bottom-panel-hide)))

(defun org-srs-mouse-mode-update-panels (&rest _)
  (if (org-srs-child-frame-p)
      (with-selected-frame (org-srs-child-frame-root)
        (org-srs-mouse-mode-update-panels-1))
    (org-srs-mouse-mode-update-panels-1)))

(add-hook 'org-srs-item-before-confirm-hook #'org-srs-mouse-mode-update-panels)
(add-hook 'org-srs-item-after-confirm-hook #'org-srs-mouse-mode-update-panels)

(provide 'org-srs-mouse)
;;; org-srs-mouse.el ends here
