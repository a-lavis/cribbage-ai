;; FILE: show.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Contains the functions related to the DEAL segment of Cribbage

;; SHOW
;; ------------------------------------------

(defun show (c)
  (let ((cut (cribbage-cut c))
        (hands (cribbage-backup-hands c)))
    (labels ((hand-score (plr)
                         (format t "~%scoring player ~A hand~%" (1+ plr))
                         (incf (svref (cribbage-score c) plr)
                               (hand-value cut (svref hands plr)))))
      (hand-score *player-one*)
      (hand-score *player-two*)
      (format t "~%scoring crib~%")
      (incf (svref (cribbage-score c) (cribbage-whose-dealer? c))
            (crib-value cut (cribbage-crib c)))
      (format t "~%")
      (print-cribbage c t 1)
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
    (format t "    flush points: ~A~%" (flush-score-hand cut hand))
    (+ (fifteen-score cards)
       (tuple-score cards)
       (run-score cards)
       (flush-score-hand cut hand))
    ))

(defun crib-value (cut crib)
  (cards-value (cons cut crib)))
(defun cards-value (cards)
  (format t "    flush points: ~A~%" (flush-score cards))
  (+ (fifteen-score cards)
     (tuple-score cards)
     (run-score cards)
     (flush-score cards))
  )


(defun power-set (listy)
  (if (null listy)
    '(())
    (let ((rest-pset (power-set (rest listy))))
      (append (mapcar (lambda (subby)
                     (cons (first listy) subby))
                   rest-pset)
              rest-pset))))

(defun fifteen-score (cards)
  (labels ((info (listy)
                 (format t "    2 points for fifteen: ~A~%" listy))
           (count-fifteens (pset)
                           (cond ((null pset) 0)
                                 ((= (apply #'+ (first pset)) 15)
                                  (info (first pset))
                                  (+ 1 (count-fifteens (rest pset))))
                                 (t (count-fifteens (rest pset))))))
    (* 2 (count-fifteens (power-set (mapcar #'card-value cards))))))

(defun tuple-score (cards)
  (let ((score-vec #('error 0 2 6 12)))
    (labels ((info (s c) (when (> s 0)
                           (format t "    ~A points for ~Aple~%" s c)))
             (count-tuples
               (ranks last-rank counter)
               (let ((score (svref score-vec counter)))
                 (cond ((null ranks) (info score counter) score)
                       ((= (first ranks) last-rank)
                        (count-tuples (rest ranks) (first ranks) (+ 1 counter)))
                       (t
                         (info score counter)
                         (+ score 
                            (count-tuples (rest ranks) (first ranks) 1)))
                       ))))
             (count-tuples (sort (mapcar #'rank-of cards) #'<) -2 1))))

(defun run-score (cards)
  (labels ((info (s rc) (when (> s 0)
                          (format t "    ~A points for ~A run~%" s rc)))
           (count-runs
             (ranks last-rank run-count multi)
             (let ((score (* (apply #'* multi)
                             (if (>= run-count 3) run-count 0))))
               (cond ((null ranks) (info score run-count) score)
                     ((= (first ranks) last-rank)
                      (count-runs (rest ranks) (first ranks)
                                  run-count (cons (1+ (first multi))
                                                  (rest multi))))
                     ((= (first ranks) (+ 1 last-rank))
                      (count-runs (rest ranks) (first ranks)
                                  (+ run-count 1) (cons 1 multi)))
                     (t
                       (info score run-count)
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
