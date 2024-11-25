;; -*- lexical-binding: t; -*-

(require 'rx)
(require 'org-element)
(require 'org-srs-log)

(defconst org-srs-item-regexp (rx "srsitem:" (group (+ (not (any ?: blank control)))) (? "::" (group (+ (not (any blank control)))))))

(defun org-srs-item-name ()
  (let ((id (org-id-get)))
    (cl-assert id)
    id))

(cl-defun org-srs-item-link (item &optional (id (org-id-get)))
  (cl-reduce
   (lambda (acc it) (format "%s::%s" acc it))
   (ensure-list item)
   :initial-value (format "srsitem:%s" id)))

(defun org-srs-item-insert (item &rest args)
  (insert "#+NAME: " (apply #'org-srs-item-link item args))
  (newline-and-indent)
  (org-srs-log-insert))

(defun org-srs-item-goto (item &rest args)
  (cl-assert (org-link-search (apply #'org-srs-item-link item args)))
  (forward-line 1))

(defun org-srs-item-repeat (item rating)
  (org-srs-item-goto item)
  (org-srs-log-repeat rating))

(defconst org-srs-item-header-regexp (rx bol (* blank) "#+NAME: " (* blank) (regexp org-srs-item-regexp) (* blank) eol))

(defun org-srs-item-from-match-data ()
  (cl-values (when (match-string 2) (mapcar #'read (split-string (match-string 2) "::"))) (match-string 1)))

(defun org-srs-item-at-point ()
  (save-excursion
    (goto-char (org-table-begin))
    (let ((element (org-element-at-point)))
      (goto-char (org-element-begin element))
      (when (re-search-forward org-srs-item-header-regexp (org-element-end element) t)
        (org-srs-item-from-match-data)))))

(cl-defgeneric org-srs-item-review (item &rest args))

(provide 'org-srs-item)
