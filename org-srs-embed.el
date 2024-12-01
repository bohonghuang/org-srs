;;; org-srs-embed.el --- Embed Org-srs entries into ordinary Org files -*- lexical-binding:t -*-

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

;; This package allows you to create and update Org-srs entries inside
;; your ordinary Org files, sparing you the maintenance of outline-based
;; Org files dedicated to review entries. All markers used to identify
;; embedded entries and cloze deletions can be prettified using
;; `org-srs-embed-overlay-mode' and are non-intrusive, which means they
;; won't pollute your Org file exports.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)

(require 'org)
(require 'org-element)
(require 'org-habit)

(require 'org-srs-property)
(require 'org-srs-item)
(require 'org-srs-item-cloze)

(cl-declaim (special org-srs-embed-overlay-mode))

(cl-defun org-srs-embed-put-cloze-overlays (&optional (start (point-min)) (end (point-max)))
  (cl-assert org-srs-embed-overlay-mode)
  (save-excursion
    (cl-loop initially (goto-char start)
             for () = (or (re-search-forward (rx "@@srs:" (group (+? anychar)) "@@") end t) (cl-return))
             for overlay = (make-overlay (match-beginning 0) (match-end 0) nil 'front-advance)
             for string = (match-string 1)
             for padding = (- (string-width (match-string 0)) (string-width (match-string 1)))
             for openp = (cl-plusp (cl-loop for char across string sum (cl-case char (?{ 1) (?} -1) (t 0))))
             do
             (overlay-put overlay 'category 'org-srs-embed-cloze)
             (overlay-put overlay 'invisible nil)
             (overlay-put overlay 'display (if (org-at-table-p) (string-pad string (+ (length string) padding) nil openp) string)))))

(cl-defun org-srs-embed-remove-cloze-overlays (&optional (start (point-min)) (end (point-max)))
  (remove-overlays start end 'category 'org-srs-embed-cloze))

(cl-defun org-srs-embed-put-meta-overlays (&optional (start (point-min)) (end (point-max)))
  (cl-assert org-srs-embed-overlay-mode)
  (save-excursion
    (goto-char start)
    (ignore-error search-failed
      (cl-loop
       (let ((case-fold-search t))
         (re-search-forward
          (rx (or (and (group-n 1 "@@comment:+srs_embedded:" (group (+? anychar)) "@@"))
                  (and bol (* blank) (group-n 1 "#+srs_embedded:" (+? anychar)) eol)))
          end))
       (let ((overlay (make-overlay
                       (match-beginning 1)
                       (match-end 1)
                       nil 'front-advance)))
         (overlay-put overlay 'category 'org-srs-embed-meta)
         (overlay-put overlay 'invisible nil)
         (overlay-put overlay 'display (propertize "#+SRS" 'face 'org-habit-ready-face)))))))

(cl-defun org-srs-embed-remove-meta-overlays (&optional (start (point-min)) (end (point-max)))
  (remove-overlays start end 'category 'org-srs-embed-meta))

(cl-defun org-srs-embed-put-overlays (&optional (start (point-min)) (end (point-max)))
  (org-srs-embed-put-cloze-overlays start end)
  (org-srs-embed-put-meta-overlays start end))

(cl-defun org-srs-embed-remove-overlays (&optional (start (point-min)) (end (point-max)))
  (org-srs-embed-remove-cloze-overlays start end)
  (org-srs-embed-remove-meta-overlays start end))

(cl-defun org-srs-embed-update-overlays (&optional (start (point-min)) (end (point-max)))
  (org-srs-embed-remove-overlays start end)
  (when org-srs-embed-overlay-mode
    (org-srs-embed-put-overlays start end)))

;;;###autoload
(define-minor-mode org-srs-embed-overlay-mode
  "Minor mode for visualizing the embedded Org-srs entries using overlays."
  :group 'org-srs
  (cl-assert (eq major-mode 'org-mode))
  (if org-srs-embed-overlay-mode (org-srs-embed-put-overlays) (org-srs-embed-remove-overlays)))

(defun org-srs-embed-cloze (start end &optional hint)
  (save-excursion
    (goto-char end)
    (if hint (insert "@@srs:}{" hint "}}@@") (insert "@@srs:}}@@"))
    (goto-char start)
    (insert "@@srs:{{@@")))

;;;###autoload
(cl-defun org-srs-embed-cloze-dwim ()
  "Like `org-srs-item-cloze', but use the non-intrusive cloze markers."
  (interactive)
  (let ((org-srs-item-cloze-function #'org-srs-embed-cloze))
    (call-interactively #'org-srs-item-cloze-dwim)))

(cl-defmethod org-srs-item-cloze-interactively :around
  (type &context (org-srs-item-cloze-function (eql #'org-srs-embed-cloze)) &optional props)
  (let* ((element (list type props))
         (start (copy-marker (org-element-begin element)))
         (end (copy-marker (org-element-end element))))
    (cl-call-next-method)
    (org-srs-embed-update-overlays start end)))

(cl-defun org-srs-embed-process-clozes (&optional (start (point-min)) (end (point-max)) (process-function #'cl-values))
  (save-excursion
    (cl-loop with start = (copy-marker start) and end = (copy-marker end)
             with regexp = (rx "@@srs:" (group (+? anychar)) "@@")
             initially (goto-char start)
             for cloze-start = (if (not (re-search-forward regexp end t))
                                   (cl-return count)
                                 (cl-assert (string-equal (match-string 1) "{{"))
                                 (prog1 (save-excursion
                                          (goto-char (match-end 0))
                                          (point-marker))
                                   (replace-match "\\1")))
             for cloze-end = (if (not (re-search-forward regexp end t))
                                 (cl-assert nil)
                               (cl-assert (string-suffix-p "}}" (match-string 1)))
                               (prog1 (save-excursion
                                        (goto-char (match-beginning 0))
                                        (point-marker))
                                 (replace-match "\\1")))
             for count from 0
             for cloze = (string-trim (buffer-substring-no-properties cloze-start cloze-end))
             do (funcall process-function cloze))))

(cl-defun org-srs-embed-cloze-bounds (&optional (position (point)))
  (save-excursion
    (goto-char position)
    (let ((regexp-left (rx "@@srs:{{")) (regexp-right (rx "}}@@")))
      (let ((bl (or (and (save-excursion (re-search-backward regexp-left (pos-bol) t)) (match-beginning 0)) (pos-bol)))
            (br (or (and (save-excursion (re-search-backward regexp-right (pos-bol) t)) (match-end 0)) (pos-bol)))
            (fl (or (and (save-excursion (re-search-forward regexp-left (pos-eol) t)) (match-beginning 0)) (pos-eol)))
            (fr (or (and (save-excursion (re-search-forward regexp-right (pos-eol) t)) (match-end 0)) (pos-eol))))
        (if (< (1- br) bl (point) fr (1+ fl)) (cons bl fr)
          (when (< (1- (point)) fl fr) (cons fl fr)))))))

(defun org-srs-embed-uncloze (start end)
  (let ((start (copy-marker start)) (end (copy-marker end)))
    (prog1 (org-srs-embed-process-clozes
            start end
            (lambda (cloze)
              (cl-assert (looking-back (rx "{{" (literal cloze) "}" (*? anychar) "}") (pos-bol)))
              (replace-match cloze)))
      (org-srs-embed-update-overlays start end))))

(cl-defmethod org-srs-item-uncloze-interactively
  (type &context ((region-active-p) (eql nil)) (org-srs-item-uncloze-function (eql #'org-srs-embed-uncloze)) &optional props)
  (if-let ((bounds (org-srs-embed-cloze-bounds)))
      (cl-destructuring-bind (start . end) bounds
        (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element start end (list type props)))))
    (cl-call-next-method)))

;;;###autoload
(cl-defun org-srs-embed-uncloze-dwim ()
  "Like `org-srs-item-uncloze', but remove the non-intrusive cloze markers."
  (interactive)
  (let ((org-srs-item-uncloze-function #'org-srs-embed-uncloze))
    (call-interactively #'org-srs-item-uncloze-dwim)))

(cl-defun org-srs-embed-srs-file-relative (&optional (root (file-name-concat org-directory "org-srs")))
  (let ((file (buffer-file-name (current-buffer))))
    (cl-assert file)
    (let ((relative (file-relative-name file org-directory)))
      (cl-assert (not (string-prefix-p ".." relative)))
      (let ((absolute (expand-file-name relative root)))
        (make-directory (file-name-directory absolute) t)
        absolute))))

(org-srs-property-defcustom org-srs-embed-srs-file #'org-srs-embed-srs-file-relative
  "A variable that determines the file to which Org-srs entries are exported."
  :group 'org-srs
  :type 'function)

(defvar org-srs-embed-export-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-srs-embed-export-finalize)
    (define-key map "\C-c\C-k" #'org-srs-embed-export-kill)
    map)
  "Keymap for `org-srs-embed-export-mode', a minor mode.")

;;;###autoload
(define-minor-mode org-srs-embed-export-mode
  "Minor mode for special key bindings in an Org-srs entry export buffer."
  :group 'org-srs
  (cl-assert (eq major-mode 'org-mode))
  (if org-srs-embed-export-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-srs-embed-export-mode-map>Org-srs entry export buffer.  Finish `\\[org-srs-embed-export-finalize]', abort `\\[org-srs-embed-export-kill]'."))
    (setq-local header-line-format nil)))

(cl-defun org-srs-embed-export-clozes (&optional (start (point-min)) (end (point-max)))
  (let ((cloze-identifier (org-srs-item-cloze-identifier)))
    (org-srs-embed-process-clozes
     start end
     (let ((identifiers (make-hash-table)))
       (lambda (cloze)
         (let ((identifier (funcall cloze-identifier cloze)))
           (cl-assert (looking-back (rx "{{" (group (*? not-newline)) (or "}}" (and "}{" (group (*? not-newline)) "}}"))) (pos-bol)))
           (goto-char (match-beginning 0))
           (forward-char 1)
           (cl-assert (null (gethash identifier identifiers)))
           (setf (gethash identifier identifiers) cloze)
           (insert "{" (org-srs-item-princ-to-string identifier) "}")))))))

(defvar-local org-srs-embed-export-window-configuration nil)

(defcustom org-srs-embed-prepare-finalize-hook nil
  "Hook that is run before the finalization starts.
The Org-srs entry export buffer is current and still narrowed."
  :group 'org-srs
  :type 'hook)

(defun org-srs-embed-export-finish ()
  (cl-assert org-srs-embed-export-mode)
  (kill-local-variable 'org-srs-embed-prepare-finalize-hook)
  (widen)
  (org-srs-embed-export-mode -1)
  (set-window-configuration (cl-shiftf org-srs-embed-export-window-configuration nil)))

(defun org-srs-embed-export-finalize ()
  "Finalize the Org-srs entry export process."
  (interactive)
  (cl-assert org-srs-embed-export-mode)
  (run-hooks 'org-srs-embed-prepare-finalize-hook)
  (org-srs-embed-export-finish))

(defun org-srs-embed-export-kill ()
  "Abort the current Org-srs entry export process."
  (interactive)
  (cl-assert org-srs-embed-export-mode)
  (widen)
  (org-back-to-heading)
  (org-cut-subtree)
  (org-srs-embed-export-finish))

(org-srs-property-defcustom org-srs-embed-export-item-type nil
  "Default item type used when exporting an Org-srs entry."
  :group 'org-srs
  :type (cons 'choice (mapcar (apply-partially #'list 'const) (org-srs-item-types))))

(cl-defgeneric org-srs-embed-export-entry (type props)
  (let* ((element (list type props))
         (buffer (current-buffer))
         (content (buffer-substring (org-element-begin element) (org-element-end element)))
         (window-configuration (current-window-configuration))
         (type (org-srs-embed-export-item-type)))
    (with-current-buffer (switch-to-buffer (find-file-noselect (funcall (org-srs-embed-srs-file))))
      (setf org-srs-embed-export-window-configuration window-configuration)
      (goto-char (point-max))
      (insert "* ")
      (org-return-and-maybe-indent)
      (org-narrow-to-subtree)
      (save-excursion
        (insert content)
        (delete-blank-lines))
      (indent-region (point) (point-max))
      (if (cl-plusp (org-srs-embed-export-clozes (point)))
          (setf type 'cloze)
        (cl-assert (not (eq type 'cloze))))
      (org-id-get-create)
      (org-back-to-heading)
      (org-end-of-line)
      (cl-assert (null org-srs-embed-prepare-finalize-hook))
      (cl-assert (not (cl-member 'org-srs-embed-prepare-finalize-hook (buffer-local-variables) :key #'car)))
      (add-hook
       'org-srs-embed-prepare-finalize-hook
       (letrec ((hook (lambda ()
                        (org-back-to-heading)
                        (if (eq type 'cloze)
                            (call-interactively #'org-srs-item-cloze-update)
                          (org-srs-item-new-interactively type))
                        (let ((id (org-id-get))
                              (front (cl-fifth (org-heading-components))))
                          (cl-assert id) (cl-assert front)
                          (with-current-buffer buffer
                            (save-excursion
                              (goto-char (org-element-begin element))
                              (let ((start (point))
                                    (indentation (rx bol (* blank))))
                                (if (not (and (looking-back indentation (pos-bol)) (looking-at indentation)))
                                    (insert (format "@@comment:+SRS_EMBEDDED: [[id:%s][%s]]@@ " id front))
                                  (let ((indentation (match-string 0)))
                                    (open-line 1)
                                    (insert indentation)
                                    (insert (format "#+SRS_EMBEDDED: [[id:%s][%s]]" id front))))
                                (org-srs-embed-update-overlays start (point))))))
                        (remove-hook 'org-srs-embed-prepare-finalize-hook hook))))
         hook)
       nil t)
      (org-srs-embed-export-mode +1))))

(defun org-srs-embed-link-file-position ()
  (cl-assert (looking-at (rx "[" "[" (group (*? anychar)) "]" (*? anychar) "]")))
  (let ((link (match-string 1)))
    (cl-assert (string-prefix-p "id:" link))
    (let ((id (string-trim-left link (rx "id:"))))
      (cl-destructuring-bind (file . position) (org-id-find id)
        (cl-values file position)))))

(cl-defun org-srs-embed-remove-comments (&optional (start (point-min)) (end (point-max)))
  (save-excursion
    (cl-loop initially (goto-char start)
             while (re-search-forward (rx "@@comment:" (*? anychar) "@@" (* blank)) end t)
             do (replace-match "")
             sum 1)))

(cl-defgeneric org-srs-embed-update-entry (type props)
  (let* ((element (list type props))
         (content (buffer-substring (org-element-begin element) (org-element-end element))))
    (cl-multiple-value-bind (file position) (org-srs-embed-link-file-position)
      (with-current-buffer (find-file-noselect file)
        (goto-char position)
        (save-restriction
          (org-narrow-to-subtree)
          (org-end-of-meta-data t)
          (delete-region (point) (point-max))
          (save-excursion (insert content))
          (indent-region (point) (point-max))
          (org-srs-embed-remove-comments (point))
          (org-srs-embed-export-clozes)
          (goto-char (point-max)))
        (delete-blank-lines)
        (org-back-to-heading)
        (call-interactively #'org-srs-item-cloze-update)))))

(defun org-srs-embed-goto-link-to-entry ()
  (cl-flet ((org-element-at-point (&aux (element (org-element-at-point)))
              (cl-case (org-element-type element)
                (plain-list (org-element-at-point (1+ (point))))
                (t element))))
    (let ((marker (point-marker))
          (current-end (org-element-end (org-element-at-point)))
          (child-start (or (ignore-errors
                             (org-down-element)
                             (org-element-begin (org-element-at-point)))
                           (point-max))))
      (cl-loop for element = (if (ignore-errors (org-backward-element) t)
                                 (or (org-element-at-point) (cl-return))
                               (when fallback-start
                                 (goto-char fallback-start))
                               (cl-return))
               for (element-start . element-end) = (cons (org-element-begin element) (org-element-end element))
               when (< element-end current-end)
               unless (and (org-at-keyword-p) (save-excursion (goto-char (1- element-end)) (org-at-keyword-p)))
               return (goto-char element-end)
               minimize element-start into fallback-start)
      (or (let ((case-fold-search t))
            (re-search-forward (rx "+srs_embedded:" (* blank)) (min current-end child-start) t))
          (null (goto-char marker))))))

(defun org-srs-embed-open-entry ()
  (cl-multiple-value-bind (file position)
      (save-excursion
        (cl-assert (org-srs-embed-goto-link-to-entry))
        (org-srs-embed-link-file-position))
    (find-file file)
    (goto-char position)))

(org-srs-property-defcustom org-srs-embed-export-front-regexp (rx (* blank) (? "- ") (group (*? anychar)) (char "：:"))
  "The default regexp used to extract the headlines of entries in batch export."
  :group 'org-srs
  :type 'regexp)

(org-srs-property-defcustom org-srs-embed-export-back-regexp (rx (+ anychar))
  "The default regexp used to extract the content of entries in batch export."
  :group 'org-srs
  :type 'regexp)

(defconst org-srs-embed-entry-header-regexp (rx (or "@@comment:+srs_embedded:" "#+srs_embedded:")))

;;;###autoload
(cl-defun org-srs-embed-dwim (arg)
  "Perform context-aware operations on the current element or embedded entry.

If the point is on the header of an already exported entry, jump to the exported
entry.
If the point is within the content of an already exported entry, update the
exported entry (note that this will overwrite the previously exported content).
If the point is on content that has not yet been exported, export the current
Org element as the entry content.
If called interactively with a `\\[universal-argument]` prefix or ARG greater
than 1 and there is an active region, perform a batch export on the region."
  (interactive "p")
  (require 'org-srs)
  (cl-flet ((ensure-entry (&optional (element (org-element-at-point)))
              (cl-assert (not (cl-find (org-element-type element) '(comment keyword))))
              (save-excursion
                (if (org-srs-embed-goto-link-to-entry)
                    (apply #'org-srs-embed-update-entry element)
                  (apply #'org-srs-embed-export-entry element))))
            (org-forward-element ()
              (cl-case (org-element-type (org-element-at-point))
                (plain-list (forward-char) (org-forward-element))
                (t (org-forward-element)))))
    (if (and (> arg 1) (region-active-p))
        (let* ((front "")
               (front-regexp (read-string "Headline regexp: " (org-srs-embed-export-front-regexp)))
               (export-hook (lambda ()
                              (when org-srs-embed-export-mode
                                (insert front)
                                (org-srs-embed-export-finalize)))))
          (unwind-protect
              (cl-loop with start = (copy-marker (region-beginning)) and end = (copy-marker (region-end))
                       initially (add-hook 'org-srs-embed-export-mode-hook export-hook) (deactivate-mark) (goto-char start)
                       do (save-excursion
                            (re-search-forward front-regexp (pos-eol))
                            (setf front (match-string 1))
                            (let ((element (org-element-copy (org-element-at-point))))
                              (setf (org-element-begin element) (point))
                              (ensure-entry element)))
                       while (progn (org-forward-element) (<= (point) (marker-position end))))
            (remove-hook 'org-srs-embed-export-mode-hook export-hook)))
      (org-srs-property-let ((org-srs-embed-export-item-type
                              (if (> arg 1) (intern (completing-read "Item type: " (org-srs-item-types) nil t))
                                (org-srs-embed-export-item-type))))
        (cond
         ((looking-at-p org-srs-embed-entry-header-regexp) (org-srs-embed-open-entry))
         ((region-active-p) (error "Exporting a region as an Org-srs entry is not yet implemented"))
         (t (ensure-entry)))))))

(provide 'org-srs-embed)
;;; org-srs-embed.el ends here
