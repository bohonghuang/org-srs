;; -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'org-element)

(defun org-srs-property-plist-at-point ()
  (save-excursion
    (when (org-at-table-p)
      (goto-char (org-table-begin)))
    (let ((element (org-element-at-point)))
      (when (eq (org-element-type element) 'table)
        (when-let ((property (org-element-property :attr_srs element)))
          (read (concat "(" (cl-reduce (lambda (acc it) (concat acc " " it)) property) ")")))))))

(defmacro org-srs-property-defcustom (name &rest defcustom-args)
  (cl-assert (string-prefix-p (symbol-name 'org-srs) (symbol-name name)))
  (let* ((property (string-remove-prefix (symbol-name 'org-) (symbol-name name)))
         (property-name (string-replace "-" "_" (upcase property)))
         (property (string-remove-prefix (symbol-name 'srs-) property))
         (propname (intern (concat ":" property))))
    (cl-with-gensyms (value anonymous-variable thunk)
      `(progn
         (defcustom ,name . ,defcustom-args)
         (cl-defun ,name (&optional ,value ,thunk)
           (if ,thunk
               (cl-locally
                (defvar ,anonymous-variable)
                (let ((,anonymous-variable ,value))
                  (funcall ,thunk)))
             (when (boundp ',anonymous-variable)
               (cl-return-from ,name (symbol-value ',anonymous-variable)))
             (let* ((,value (cl-getf (org-srs-property-plist-at-point) ,propname ',anonymous-variable)))
               (unless (eq ,value ',anonymous-variable)
                 (cl-return-from ,name ,value)))
             (when-let ((,value (org-entry-get nil ,property-name t t)))
               (cl-return-from ,name (read ,value)))
             ,name))))))

(defmacro org-srs-property-let (bindings &rest body)
  (declare (indent 1))
  (if bindings
      (cl-destructuring-bind ((var val) . rest) bindings
        `(,var ,val (lambda () (org-srs-property-let ,rest . ,body))))
    `(progn . ,body)))

(provide 'org-srs-property)
;;; org-srs-property.el ends here
