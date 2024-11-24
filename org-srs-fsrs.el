;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'fsrs)
(require 'org-srs-time)
(require 'org-srs-algorithm)

(cl-defmethod org-srs-algorithm-repeat ((fsrs fsrs) (args null))
  (ignore fsrs args)
  (let ((card (make-fsrs-card)))
    `((stability . ,(fsrs-card-stability card))
      (difficulty . ,(fsrs-card-difficulty card))
      (state . ,(fsrs-card-state card)))))

(defconst org-srs-fsrs-card-slots (mapcar #'cl--slot-descriptor-name (cl--class-slots (cl-find-class 'fsrs-card))))

(cl-defmethod org-srs-algorithm-repeat ((fsrs fsrs) (args list))
  (let ((card (make-fsrs-card))
        (rating (alist-get 'rating args))
        (timestamp (alist-get 'timestamp args (org-srs-timestamp-now))))
    (cl-assert (keywordp rating))
    (setf (car (cl-find 'timestamp args :key #'car :from-end t)) 'last-review)
    (cl-loop for slot in org-srs-fsrs-card-slots
             for cons = (assoc slot args)
             for (key . value) = cons
             when cons
             do (setf (eieio-oref card key) value))
    (cl-loop with card = (fsrs-scheduling-info-card (cl-getf (fsrs-repeat fsrs card timestamp) rating))
             for slot in org-srs-fsrs-card-slots
             collect (cons (cl-case slot (due 'timestamp) (t slot)) (eieio-oref card slot)) into slots
             finally (cl-return (nconc slots args)))))

(unless org-srs-algorithm
  (setf org-srs-algorithm (make-fsrs)))

(provide 'org-srs-fsrs)
