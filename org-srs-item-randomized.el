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
(require 'org-srs-review)

;;; Core Functions

(defun org-srs-item-card-randomized-parse-candidates ()
  "Parse all valid candidate pairs in the current subtree.
Performs a single tree traversal and returns a list of plists
\(:candidate (BEG . END) :front (BEG . END) :back (BEG . END))
for each candidate heading that contains at least two subheadings."
  (let ((top-level (or (org-current-level) 0))
        (pending nil)
        (result nil))
    (org-map-entries
     (lambda ()
       (let ((level (org-current-level)))
         (cond
          ((= level (1+ top-level))
           ;; Finalize the previous candidate if valid.
           (when (and pending
                      (plist-get pending :front)
                      (plist-get pending :back))
             (push (list :candidate (cons (plist-get pending :candidate-beg)
                                          (plist-get pending :candidate-end))
                         :front (plist-get pending :front)
                         :back (plist-get pending :back))
                   result))
           ;; Start accumulating a new candidate.
           (setq pending
                 (list :candidate-beg (point)
                       :candidate-end (save-excursion (org-end-of-subtree t) (point))
                       :subheading-count 0
                       :front nil
                       :back nil)))
          ((and pending (= level (+ 2 top-level)))
           (cl-incf (plist-get pending :subheading-count))
           (cl-case (plist-get pending :subheading-count)
             (1 (plist-put pending :front
                           (cons (point) (org-srs-entry-end-position))))
             (2 (plist-put pending :back
                           (cons (point) (org-srs-entry-end-position)))))))))
     nil 'tree)
    ;; Finalize the last candidate.
    (when (and pending
               (plist-get pending :front)
               (plist-get pending :back))
      (push (list :candidate (cons (plist-get pending :candidate-beg)
                                   (plist-get pending :candidate-end))
                   :front (plist-get pending :front)
                   :back (plist-get pending :back))
            result))
    (nreverse result)))

(defun org-srs-item-card-randomized-put-invisible-overlay (start end)
  "Create an invisible overlay from START to END."
  (let ((overlay (make-overlay start end nil 'front-advance)))
    (overlay-put overlay 'category 'org-srs-item-card-randomized)
    (overlay-put overlay 'invisible t)))

(cl-defun org-srs-item-card-randomized-remove-overlays (&optional (start (point-min)) (end (point-max)))
  "Remove all randomized card overlays between START and END."
  (remove-overlays start end 'category 'org-srs-item-card-randomized))

(defvar-local org-srs-item-card-randomized--selected-candidate nil
  "The plist of the currently selected candidate for this review session.")

(defun org-srs-item-card-randomized-hide-pairs-except (selected candidates)
  "Hide all CANDIDATES except SELECTED using invisible overlays.
SELECTED is the chosen plist from CANDIDATES.
For SELECTED, show front and hide back.
For all others, hide the entire candidate heading and contents."
  (setq org-srs-item-card-randomized--selected-candidate selected)
  (dolist (candidate candidates)
    (if (eq candidate selected)
        ;; Selected pair: hide only the back.
        (let ((back (plist-get candidate :back)))
          (org-srs-item-card-randomized-put-invisible-overlay (car back) (cdr back)))
      ;; Other pairs: hide entire candidate heading and its contents.
      (let* ((region (plist-get candidate :candidate))
             (start (max (point-min) (1- (car region)))))
        (org-srs-item-card-randomized-put-invisible-overlay start (cdr region))))))

(defun org-srs-item-card-randomized-show ()
  "Reveal the selected pair's back, keeping other candidates hidden."
  (save-excursion
    (org-fold-show-subtree)
    ;; Remove only the overlay hiding the selected pair's back.
    (when org-srs-item-card-randomized--selected-candidate
      (let ((back (plist-get org-srs-item-card-randomized--selected-candidate :back)))
        (org-srs-item-card-randomized-remove-overlays (car back) (cdr back))))))

;;; Review Method

(cl-defmethod org-srs-item-review ((type (eql 'card-randomized)) &rest args)
  (cl-destructuring-bind (&optional (side 'back)) args
    (cl-ecase side
      (back
       (let* ((candidates (org-srs-item-card-randomized-parse-candidates))
              (count (length candidates))
              (selected (progn
                          (cl-assert (> count 0) nil "No valid Front/Back pairs found")
                          (nth (random count) candidates)))
              (buffer (current-buffer)))
         (org-srs-item-narrow)
         (org-fold-show-all)
         (org-srs-item-card-randomized-remove-overlays)
         (org-srs-item-card-randomized-hide-pairs-except selected candidates)
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
