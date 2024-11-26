;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-srs-log)
(require 'org-srs-query)

(defcustom org-srs-new-items-per-day 20 "")
(defcustom org-srs-max-reviews-per-day 200 "")
(defcustom org-srs-new-items-ignore-review-limit-p nil "")

(cl-defun org-srs-review (&optional (source (buffer-file-name)))
  (cl-etypecase source
    (string
     (cl-assert (file-exists-p source))
     (cl-assert (not (file-directory-p source)))
     (with-current-buffer (find-file-noselect source)
       (let ((learned-items (org-srs-query-buffer (org-srs-query-predicate-learned)))
             (reviewed-items (org-srs-query-buffer (org-srs-query-predicate-reviewed))))
         (org-srs-query-buffer
          (if (< (length reviewed-items) org-srs-max-reviews-per-day)
              (if (< (length learned-items) org-srs-new-items-per-day)
                  (org-srs-query-predicate-due)
                (org-srs-query-predicate-and
                 (org-srs-query-predicate-due)
                 (org-srs-query-predicate-not
                  (org-srs-query-predicate-new))))
            (if (< (length learned-items) org-srs-new-items-per-day)
                (if org-srs-new-items-ignore-review-limit-p
                    (org-srs-query-predicate-and
                     (org-srs-query-predicate-due)
                     (org-srs-query-predicate-new))
                  (cl-return-from org-srs-review nil))
              (cl-return-from org-srs-review nil)))))))))

(provide 'org-srs-review)
