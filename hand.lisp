(defun make-hand (c1 c2 c3)
  (list c1 c2 c3))

(defun hand-value (cut hand)
  (let ((cards (cons cut hand)))
    (+ (fifteen-score cards)
       (tuple-score cards)
       (run-score cards)
       (flush-score cut hand))))

(defun power-set (listy)
  (if (null listy)
    '(())
    (let ((rest-pset (power-set (rest listy))))
      (append (mapcar (lambda (subby)
                     (cons (first listy) subby))
                   rest-pset)
              rest-pset))))

(defun fifteen-score (cards)
  (labels ((count-fifteens (pset)
                           (cond ((null pset) 0)
                                 ((= (apply #'+ (first pset)) 15)
                                  (+ 1 (count-fifteens (rest pset))))
                                 (t (count-fifteens (rest pset))))))
    (* 2 (count-fifteens (power-set (mapcar #'card-value cards))))))

#|
(defun fifteen-score-2 (cards)
  (labels ((count-15s
             (c-vals)
             (if (null c-vals)
               0
               (let ((rest-15s (count-15s (rest c-vals))))
                 (+ (mapcar (lambda (subby)
                              (+ (first c-vals) subby)))))))))
|#

(defun tuple-score (cards)
  (let ((score-vec #('error 0 2 6 12)))
    (labels ((count-tuples
               (ranks last-rank counter)
               (cond ((null ranks) (svref score-vec counter))
                     ((= (first ranks) last-rank)
                      (count-tuples (rest ranks) (first ranks) (+ 1 counter)))
                     (t
                       (+ (svref score-vec counter)
                          (count-tuples (rest ranks) (first ranks) 1)))
                     )))
      (count-tuples (sort (mapcar #'rank-of cards) #'<) -2 1))))

;; doesn't work properly for '(1 2 2 3 3)...
;; when there is more than one tuple in a run
(defun run-score (cards)
  (labels ((count-runs
             (ranks last-rank run-count multi)
             (cond ((null ranks) (* multi (if (>= run-count 3) run-count 0)))
                   ((= (first ranks) last-rank)
                    (count-runs (rest ranks) (first ranks)
                               run-count (+ multi 1)))
                   ((= (first ranks) (+ 1 last-rank))
                    (count-runs (rest ranks) (first ranks)
                               (+ run-count 1) multi))
                   (t
                     (+ (* multi (if (>= run-count 3) run-count 0))
                        (count-runs (rest ranks) (first ranks) 1 1)))
                   )))
    (count-runs (sort (mapcar #'rank-of cards) #'<) -2 1 1)))
#|
(labels ((count-runs
           (ranks last-rank counter)
           (cond ((null ranks) counter)
                 ((=
|#
#|
(defun flush-score (cards)
  (labels ((
            |#
