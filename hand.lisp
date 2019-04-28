(defun make-hand (c1 c2 c3)
  (list c1 c2 c3))

(defun hand-value (cut hand)
  (let ((cards (cons cut hand)))
    (+ (fifteen-score cards)
       (tuple-score cards)
       (run-score cards)
       (flush-score cards))))

(defun power-set (listy)
  (if (null listy)
    listy
    (let ((rest-pset (power-set (rest listy))))
      (append (mapcar (lambda (subby)
                     (cons (first listy) subby))
                   rest-pset)
              rest-pset))))

(defun fifteen-score (cards)
  (labels ((count-fifteens (pset)
                           (cond ((null pset) 0)
                                 ((= (apply #'+ hand) 15)
                                  (+ 1 (count-fifteens (rest pset))))
                                 (else (count-fifteens (rest pset))))))
    (count-fifteens (power-set (mapcar #'card-value cards)))))

#|
(defun fifteen-score (cards)
  (labels ((count-fifteens
             (c-vals)
             (if (null c-vals)
               0
               (let ((rest-15s (count-15s (rest c-vals))))
                 (+ ())))))))
|#

#|  WORK IN PROGRESS
(defun tuple-score (cards)
  (labels ((count-tuples
             (ranks counter current-rank)
             (cond ((null ranks) 0)
                   ((= (first ranks) current-rank)
                    (count-tuples (rest ordered) (+ 1 counter) current-rank))
                   ((= counter 2)
                    (+ 2 (count-tuples (rest ordered) 0 )))
                   )))
    (let ((ranks (sort (mapcar #'rank-of cards) #'<)))
      (count-tuples (rest ranks) 1 (first ranks)))))
|#
