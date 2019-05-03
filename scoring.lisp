;; FILE: cards.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Methods in FILE
;; --------------------------
;; SCORING, HIS-HEELS, GO, FIFTEEN, PAIR, TRIPLE, QUADRUPLE, RUN,
;;   FLUSH, HIS-KNOBS

;; using pair to represent players' scores (p1 p2)
;; convention for keeping previous score doesn't seem to have effect on game


;; PILE-SCORE
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: accumulation of points to be added to CRIBBAGE-SCORE

(defun pile-score (c)
  ;; get PLR's score
  (let* ((plr (cribbage-whose-turn? c))
         (go-plr (switch plr))
         (pile (cribbage-pile c)))
    ;; call GO-SCORE and add to other player's score
    (setf (svref (cribbage-score c) go-plr)
      (+ (svref (cribbage-score c) go-plr)
        (go-score (cribbage-pile c) (svref (cribbage-plr-hands c) plr))))
    ;; accumulate all possible scoring opportunities
    (+
      (his-knobs pile (suit-of (cribbage-cut c)))
      (fifteen (pile-sum pile))
      (thirty-one (pile-sum pile))
      (n-of-a-kind pile)
      (run pile))))


;; ====================================
;; evaluated during DEAL
;; ====================================

;; HIS-HEELS
;; ------------------------------------------
;; INPUTS: CUT, the card flipped over after the CUT
;; OUTPUTS: 2
;; CONDITION: when the card placed on top of the deck is a JACK of any SUIT

(defun his-heels (cut)
    ;; if CUT == JACK, return 2, else 0
    (if (equal (rank-of cut) *jack*) 2 0))

;; ====================================
;; evaluated during PLAY
;; ====================================

;; HIS-KNOBS
;; ------------------------------------------
;; INPUTS: PILE, the pile of cards
;;         CUT-SUIT, the SUIT of the card placed after the CUT
;; OUTPUTS: 1
;; CONDITION: the TOP-CARD matches the CUT-SUIT, only on first card

(defun his-knobs (pile cut-suit)
  ;; get TOP-CARD
  (let ((top-card (first pile)))
    ;; if TOP-CARD == CUT-SUIT  && length(pile) == 1
    (when (and (equal (length pile) 1)
               (equal (suit-of top-card) cut-suit))
      ;; return value of this scoring opportunity
      (return-from his-knobs 1)))
  ;; otherwise return 0
  0)


;; GO-SCORE  -- scored retroactively
;; ------------------------------------------
;; INPUTS: PILE-SUM, the sum of the cards placed in the PILE
;;         CURR-PLR-HAND, the current player's hand
;; OUTPUTS: 1
;; CONDITION: when the next player can't play a card because the PILE-SUM
;;    is already 31 or would go over

(defun go-score (pile curr-plr-hand)
  ;; iterate thru CURR-PLR-HAND
  (dolist (card curr-plr-hand)
    ;; CARD-VALUE + PILE-SUM < 31
    (when (<= (+ (card-value card) (pile-sum pile)) 31)
      (return-from go-score 0)))
  ;; otherwise have gone through CURR-PLR-HAND and no possibilities
  1)


;; FIFTEEN
;; ------------------------------------------
;; INPUTS: PILE-SUM, the sum of the cards placed in the PILE
;; OUTPUTS: 2
;; CONDITION: when the PILE-SUM reaches 15

(defun fifteen (pile-sum)
  ;; if PILE-SUM == 15
  (if (equal pile-sum 15) 2 0))


;; THIRTY-ONE
;; ------------------------------------------
;; INPUTS: PILE-SUM, the sum of the cards placed in the PILE
;; OUTPUTS: 2
;; CONDITION: when the PILE-SUM reaches 31

(defun thirty-one (pile-sum)
  ;; if PILE-SUM == 31
  (if (equal pile-sum 31) 2 0))


;; N-OF-A-KIND
;; ------------------------------------------
;; INPUTS: PILE, the pile of CARDS
;; OUTPUTS: scoring for PAIR, TRIPLE, or QUADRUPLE (exclusive)

(defun n-of-a-kind (pile)
  (cond
    ;; score a QUADRUPLE
    ((quadruple? pile) 12)
    ;; score a TRIPLE
    ((triple? pile) 6)
    ;; score a PAIR
    ((pair? pile) 2)
    ;; else 0
    (t 0)))


;; PAIR?
;; ------------------------------------------
;; INPUTS: PILE, the pile of cards
;; OUTPUTS: Boolean, T if a pair

(defun pair? (pile)
  ;; top two cards in pile are equal
  (and (>= (length pile) 2)
           (= (rank-of (first pile)) (rank-of (second pile)))))


;; TRIPLE?
;; ------------------------------------------
;; INPUTS: PILE, the pile of cards
;; OUTPUTS: Boolean, T if a triple

(defun triple? (pile)
  ;; top three cards in pile are equal
  (and (>= (length pile) 3)
           (= (rank-of (first pile))
                  (rank-of (second pile))
                  (rank-of (third pile)))))


;; QUADRUPLE
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLR, the last player to place a CARD
;; OUTPUTS: Boolean, T if a quadruple

(defun quadruple? (pile)
  ;; top four cards in pile are equal
  (and (>= (length pile) 4)
           (= (rank-of (first pile))
                  (rank-of (second pile))
                  (rank-of (third pile))
                  (rank-of (fourth pile)))))


;; RUN
;; ------------------------------------------
;; INPUTS: PILE, the pile of cards
;; OUTPUTS: the number of cards in the run (if a run is present)
;; CONDITION: the last three or more cards have continuous
;;    rank (can be out of order)

(defun run (pile)
  ;; get sorted lists of max length 3,4,5
  (let* ((potential-five (subseq pile 0 (min (length pile) 5)))
         (potential-four (subseq pile 0 (min (length pile) 4)))
         (potential-three (subseq pile 0 (min (length pile) 3)))
         (sorted-five (sort (mapcar #'rank-of potential-five) #'<))
         (sorted-four (sort (mapcar #'rank-of potential-four) #'<))
         (sorted-three (sort (mapcar #'rank-of potential-three) #'<)))
    (cond
      ;; a RUN of 5
      ((and (succession? sorted-five)
            (equal (length sorted-five) 5))
        5)
      ;; a RUN of 4
      ((and (succession? sorted-four)
            (equal (length sorted-four) 4))
        4)
      ;; a RUN of 3
      ((and (succession? sorted-three)
            (equal (length sorted-three) 3))
        3)
      ;; no RUN
      (t 0))))


;; SUCCESSION?
;; ------------------------------------------
;; INPUTS: SORTED, a sorted list
;; OUTPUTS: Boolean, T if difference between all cards is 1

(defun succession? (sorted)
  (or (null (rest sorted))
    (and (equal (first sorted) (- (second sorted) 1))
         (succession? (rest sorted)))))
