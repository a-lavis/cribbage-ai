(defun show (c)
  (let ((cut (cribbage-cut c))
        (plr-hands (cribbage-plr-hands c)))
    (labels ((hand-score (plr)
                    (incf (svref (cribbage-score c) plr)
                          (hand-value cut (svref plr-hands plr)))))
      (hand-score *player-one*)
      (hand-score *player-two*)
      (incf (svref (cribbage-score c) (cribbage-whose-dealer? c))
            (crib-value cut (cribbage-crib c)))
      )))


;; -------------------------------------------------------------------------- ;;
;; ------------------------- card-scoring functions ------------------------- ;;
;; -------------------------------------------------------------------------- ;;

(defun make-hand (c1 c2 c3)
  (list c1 c2 c3))
(defun make-crib (c1 c2 c3 c4)
  (list c1 c2 c3 c4))

(defun print-cards (cards)
  (print-card (first cards))
  (print-cards (rest cards)))

(defun hand-value (cut hand)
  (let ((cards (cons cut hand)))
    (+ (fifteen-score cards)
       (tuple-score cards)
       (run-score cards)
       (flush-score-hand cut hand))))

(defun crib-value (cut crib)
  (cards-value (cons cut crib)))
(defun cards-value (cards)
  (+ (fifteen-score cards)
     (tuple-score cards)
     (run-score cards)
     (flush-score cards)))


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

(defun run-score (cards)
  (labels ((count-runs
             (ranks last-rank run-count multi)
             (let ((score (* (apply #'* multi)
                             (if (>= run-count 3) run-count 0))))
               (cond ((null ranks) score)
                     ((= (first ranks) last-rank)
                      (count-runs (rest ranks) (first ranks)
                                  run-count (cons (1+ (first multi))
                                                  (rest multi))))
                     ((= (first ranks) (+ 1 last-rank))
                      (count-runs (rest ranks) (first ranks)
                                  (+ run-count 1) (cons 1 multi)))
                     (t
                       (+ score 
                          (count-runs (rest ranks) (first ranks) 1 '(1))))
                     ))))
    (count-runs (sort (mapcar #'rank-of cards) #'<) -2 1 '(1))))

(defun flush-score-hand (cut hand)
  (let ((suits (mapcar #'suit-of hand)))
    (if (apply #'= suits)
      (+ (length hand)
         (if (= (suit-of cut) (first suits))
           1
           0))
      0)))

(defun flush-score (cards)
  (if (apply #'= (mapcar #'suit-of cards))
    (length cards)
    0))
