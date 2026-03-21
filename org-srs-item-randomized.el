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

(require 'org)

(require 'org-srs-entry)
(require 'org-srs-item)
(require 'org-srs-review)

;;; Core Functions

(defun org-srs-item-card-randomized-parse-candidates ()
  "Parse all valid candidate pairs in the current subtree.
Performs a single tree traversal and returns a list where each
element is (CANDIDATE-REGION FRONT-REGION BACK-REGION), with each
region a cons cell (BEG . END)."
  (let ((top-level (or (org-current-level) 0))
        (candidate-beg nil)
        (candidate-end nil)
        (front nil)
        (back nil)
        (subheading-count 0)
        (result nil))
    (org-map-entries
     (lambda ()
       (let ((level (org-current-level)))
         (cond
          ((= level (1+ top-level))
           (when (and front back)
             (push (list (cons candidate-beg candidate-end) front back) result))
           (setf candidate-beg (point)
                 candidate-end (save-excursion (org-end-of-subtree t) (point))
                 front nil
                 back nil
                 subheading-count 0))
          ((and candidate-beg (= level (+ 2 top-level)))
           (cl-incf subheading-count)
           (cl-case subheading-count
             (1 (setf front (cons (point) (org-srs-entry-end-position))))
             (2 (setf back (cons (point) (org-srs-entry-end-position)))))))))
     nil 'tree)
    (when (and front back)
      (push (list (cons candidate-beg candidate-end) front back) result))
    (nreverse result)))

(defun org-srs-item-card-randomized-put-invisible-overlay (start end)
  "Create an invisible overlay from START to END."
  (let ((overlay (make-overlay start end nil 'front-advance)))
    (overlay-put overlay 'category 'org-srs-item-card-randomized)
    (overlay-put overlay 'invisible t)))

(cl-defun org-srs-item-card-randomized-remove-overlays (&optional (start (point-min)) (end (point-max)))
  "Remove all randomized card overlays between START and END."
  (remove-overlays start end 'category 'org-srs-item-card-randomized))

(defun org-srs-item-card-randomized-hide-pairs-except (selected candidates)
  "Hide all CANDIDATES except SELECTED using invisible overlays.
SELECTED and each element of CANDIDATES are (CANDIDATE FRONT BACK) lists.
For SELECTED, show front and hide back.
For all others, hide the entire candidate heading and contents."
  (dolist (candidate candidates)
    (if (eq candidate selected)
        (cl-destructuring-bind (_candidate _front (back-beg . back-end)) candidate
          (org-srs-item-card-randomized-put-invisible-overlay back-beg back-end))
      (cl-destructuring-bind ((cand-beg . cand-end) _front _back) candidate
        (org-srs-item-card-randomized-put-invisible-overlay
         (max (point-min) (1- cand-beg)) cand-end)))))

(defun org-srs-item-card-randomized-show (selected)
  "Reveal the back of SELECTED, keeping other candidates hidden.
SELECTED is a (CANDIDATE FRONT BACK) list."
  (save-excursion
    (org-fold-show-subtree)
    (cl-destructuring-bind (_candidate _front (back-beg . back-end)) selected
      (org-srs-item-card-randomized-remove-overlays back-beg back-end))))

;;; Review Method

(cl-defmethod org-srs-item-review ((type (eql 'card-randomized)) &rest args)
  "Method to review an item of TYPE `card-randomized' with ARGS."
  (cl-destructuring-bind (&optional (side 'back)) args
    (cl-ecase side
      (back
       (let* ((candidates (org-srs-item-card-randomized-parse-candidates))
              (count (length candidates))
              (selected (progn
                          (cl-assert (> count 0) nil "No valid Front/Back pairs found")
                          (nth (random count) candidates))))
         (org-srs-item-narrow)
         (org-fold-show-subtree)
         (org-srs-item-card-randomized-remove-overlays)
         (org-srs-item-card-randomized-hide-pairs-except selected candidates)
         (org-srs-item-add-hook-once
          'org-srs-item-after-confirm-hook
          (org-srs-review-item-hook
           (lambda () (org-srs-item-card-randomized-show selected))))
         (org-srs-item-add-hook-once
          'org-srs-review-continue-hook
          #'org-srs-item-card-randomized-remove-overlays 10)
         (apply (org-srs-item-confirm) type args))))))

;;; Creation Method

(cl-defmethod org-srs-item-new ((_type (eql 'card-randomized)) &rest args)
  "Method for creating a new randomized flashcard with ARGS."
  (cl-assert (null args))
  (org-srs-item-new '(card-randomized back)))

(provide 'org-srs-item-card-randomized)
;;; org-srs-item-card-randomized.el ends here
