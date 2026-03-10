;;; org-srs-item-card-randomized.el --- Randomized flashcard item type -*- lexical-binding: t -*-

;; This file is an extension to org-srs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This package implements randomized flashcard review items in Org-srs.
;; Each card can have multiple front/back pairs, and one is randomly
;; selected during each review session.
;;
;; Format: Intermediate subheadings containing Front/Back subheadings
;; Item link: srsitem:ID::card-randomized::back

;;; Code:

(require 'cl-lib)
(require 'cl-generic)

(require 'org-srs-item)
(require 'org-srs-item-card)
(require 'org-srs-review)

(defgroup org-srs-item-card-randomized nil
  "Randomized flashcard type for review items."
  :group 'org-srs-item
  :prefix "org-srs-item-card-randomized-")

;;; Core Functions

(defun org-srs-item-card-randomized-get-candidate-headings ()
  "Get all intermediate candidate headings at level+1.
Returns a list of positions for each candidate heading."
  (let ((level (or (org-current-level) 0))
        (candidates nil))
    (org-map-entries
     (lambda ()
       (when (= (org-current-level) (1+ level))
         (push (point) candidates)))
     nil 'tree)
    (nreverse candidates)))

(defun org-srs-item-card-randomized-regions (index)
  "Get the front and back regions for the pair at INDEX.
INDEX is 1-based. Returns (VALUES front-region back-region) or nil if INDEX is invalid."
  (let ((candidates (org-srs-item-card-randomized-get-candidate-headings)))
    (when (and (>= index 1) (<= index (length candidates)))
      (save-excursion
        (goto-char (nth (1- index) candidates))
        (let ((level (org-current-level))
              (front nil)
              (back nil)
              (count 0))
          (org-map-entries
           (lambda ()
             (when (= (org-current-level) (1+ level))
               (cl-incf count)
               (cond
                ((= count 1)
                 (setf front (cons (point) (org-srs-entry-end-position))))
                ((= count 2)
                 (setf back (cons (point) (org-srs-entry-end-position)))))))
           nil 'tree)
          (when (and front back)
            (cl-values front back)))))))

(defun org-srs-item-card-randomized-count-pairs ()
  "Count the number of complete front/back pairs available.
Returns the number of intermediate headings that contain at least 2 subheadings."
  (let ((candidates (org-srs-item-card-randomized-get-candidate-headings))
        (valid-count 0))
    (dolist (pos candidates)
      (save-excursion
        (goto-char pos)
        (let ((level (org-current-level))
              (subheading-count 0))
          (org-map-entries
           (lambda ()
             (when (= (org-current-level) (1+ level))
               (cl-incf subheading-count)))
           nil 'tree)
          (when (>= subheading-count 2)
            (cl-incf valid-count)))))
    valid-count))

(defun org-srs-item-card-randomized-select-index ()
  "Select a random valid pair index.
Returns a number from 1 to the count of available pairs."
  (let ((count (org-srs-item-card-randomized-count-pairs)))
    (cl-assert (> count 0) nil "No valid Front/Back pairs found")
    (1+ (random count))))

(defun org-srs-item-card-randomized-all-pair-regions ()
  "Get all Front/Back pair regions.
Returns a list of (INDEX FRONT-REGION BACK-REGION) for each valid pair."
  (let ((count (org-srs-item-card-randomized-count-pairs)))
    (cl-loop for i from 1 to count
             collect (cl-multiple-value-bind (front back)
                         (org-srs-item-card-randomized-regions i)
                       (list i front back)))))

(defun org-srs-item-card-randomized-put-invisible-overlay (start end)
  "Create an invisible overlay from START to END."
  (let ((overlay (make-overlay start end nil 'front-advance)))
    (overlay-put overlay 'category 'org-srs-item-card-randomized)
    (overlay-put overlay 'invisible t)))

(cl-defun org-srs-item-card-randomized-remove-overlays (&optional (start (point-min)) (end (point-max)))
  "Remove all randomized card overlays between START and END."
  (remove-overlays start end 'category 'org-srs-item-card-randomized))

(defvar-local org-srs-item-card-randomized--selected-index nil
  "The currently selected pair index for this review session.")

(defun org-srs-item-card-randomized-hide-pairs-except (selected-index)
  "Hide all pairs except SELECTED-INDEX using invisible overlays.
For SELECTED-INDEX, show front and hide back."
  (setq org-srs-item-card-randomized--selected-index selected-index)
  (let ((pairs (org-srs-item-card-randomized-all-pair-regions)))
    (dolist (pair pairs)
      (cl-destructuring-bind (index front back) pair
        (if (= index selected-index)
            ;; Selected pair: hide only the back
            (cl-destructuring-bind (back-beg . back-end) back
              (org-srs-item-card-randomized-put-invisible-overlay back-beg back-end))
          ;; Other pairs: hide completely (both front and back)
          (cl-destructuring-bind (front-beg . front-end) front
            (cl-destructuring-bind (back-beg . back-end) back
              ;; Hide from start of front to end of back
              (org-srs-item-card-randomized-put-invisible-overlay (1- front-beg) back-end))))))))

(defun org-srs-item-card-randomized-show ()
  "Show only the selected pair completely, keeping others hidden."
  (save-excursion
    (org-fold-show-subtree)
    ;; Remove only the overlay hiding the selected pair's back
    (when org-srs-item-card-randomized--selected-index
      (cl-multiple-value-bind (front back)
          (org-srs-item-card-randomized-regions org-srs-item-card-randomized--selected-index)
        (cl-destructuring-bind (back-beg . back-end) back
          (org-srs-item-card-randomized-remove-overlays back-beg back-end))))))

;;; Review Method

(cl-defmethod org-srs-item-review ((type (eql 'card-randomized)) &rest args)
  (cl-destructuring-bind (&optional (side 'back)) args
    (cl-ecase side
      (back
       (let ((index (org-srs-item-card-randomized-select-index))
             (buffer (current-buffer)))
         (org-srs-item-narrow)
         (org-fold-show-all)
         (org-srs-item-card-randomized-remove-overlays)
         (org-srs-item-card-randomized-hide-pairs-except index)
	 (let ((cleanup-cell (cons nil nil)))
	   (setcar cleanup-cell
		   (lambda ()
		     (when (buffer-live-p buffer)
		       (with-current-buffer buffer
			 (org-srs-item-card-randomized-remove-overlays)))
		     (remove-hook 'org-srs-review-finish-hook (car cleanup-cell))
		     (remove-hook 'org-srs-review-continue-hook (car cleanup-cell))))
	   (org-srs-item-add-hook-once
	    'org-srs-item-after-confirm-hook
	    (org-srs-review-item-hook
	     (lambda ()
	       (org-srs-item-card-randomized-show)
	       (org-srs-item-add-hook-once
		'org-srs-item-after-rate-hook
		(org-srs-review-item-hook (car cleanup-cell))))))
	   (add-hook 'org-srs-review-finish-hook (car cleanup-cell))
	   (add-hook 'org-srs-review-continue-hook (car cleanup-cell)))	 
         (apply (org-srs-item-confirm) type args))))))

;;; Creation Method

(cl-defmethod org-srs-item-new ((_type (eql 'card-randomized)) &rest args)
  "Create a new randomized flashcard with ARGS."
  (cl-assert (null args))
  (org-srs-item-new '(card-randomized back)))

(provide 'org-srs-item-card-randomized)
;;; org-srs-item-card-randomized.el ends here
