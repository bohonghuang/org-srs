;; -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-srs-algorithm)
(require 'org-srs-time)
(require 'org-srs-table)

(defun org-srs-log-insert ()
  (let ((initargs (org-srs-algorithm-repeat (org-srs-algorithm) nil)))
    (org-srs-table-from-alist
     `((timestamp . ,(org-srs-timestamp-now)) (rating . ""). ,initargs))
    (org-srs-table-forward-star)
    (setf (org-srs-table-field 'timestamp) (org-srs-timestamp-now))
    (org-table-align)))

(cl-defun org-srs-log-repeat (rating &aux (args nil))
  (cl-assert (org-at-table-p))
  (org-srs-table-goto-starred-line)
  (forward-line -1)
  (unless (org-at-table-hline-p)
    (setf args (nconc (org-srs-table-current-line) args)))
  (org-srs-table-goto-starred-line)
  (setf (org-srs-table-field 'timestamp) (org-srs-timestamp-now)
        args (nconc (org-srs-table-current-line) args))
  (cl-loop initially (setf args (org-srs-algorithm-repeat (org-srs-algorithm) (cl-acons 'rating rating args)))
           for (name . nil) in (org-srs-table-column-name-number-alist)
           for field = (alist-get name args)
           unless (eq name 'timestamp)
           do (setf (org-srs-table-field name) (prin1-to-string field t))
           finally (org-srs-table-forward-star) (setf (org-srs-table-field 'timestamp) (alist-get 'timestamp args)))
  (org-table-align))

(defconst org-srs-log-latest-timestamp-regexp (rx (regexp org-srs-table-starred-line-regexp) (* blank) (group (regexp org-srs-timestamp-regexp))))

(defcustom org-srs-log-drawer-name "SRSITEMS" "")

(defun org-srs-log-end-of-drawer ()
  (save-restriction
    (org-narrow-to-subtree)
    (org-back-to-heading)
    (if (re-search-forward (rx bol (* blank) ":" (literal org-srs-log-drawer-name) ":" (* blank) eol) nil t)
        (progn
          (goto-char (org-element-end (org-element-at-point)))
          (re-search-backward (rx bol (* blank) ":END:" (* blank) eol)))
      (org-end-of-meta-data t)
      (unless (looking-back (rx bol (* blank)) (pos-bol))
        (org-return))
      (insert ":" org-srs-log-drawer-name ":")
      (org-return)
      (insert ":END:")
      (beginning-of-line))))

(provide 'org-srs-log)
