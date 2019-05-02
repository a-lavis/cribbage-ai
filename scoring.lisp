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
;; OUTPUTS: the new score for PLR after all possible scoring opportunities
;;       have been considered

(defun pile-score (c)
  ;; get PLR's score
  (let* ((plr (cribbage-whose-turn? c))
         (plr-score (svref (cribbage-score c) plr)))
    ;; call GO-SCORE and add to other player's score
    (setf (svref (cribbage-score c) (switch plr))
      (+ (svref (cribbage-score c) (switch plr))
        (go-score (cribbage-pile c) plr)))
    ;; call other scoring fxns and add to current player's score
    (+
      (his-knobs (first (cribbage-pile c)) (suit-of (cribbage-cut c)))
      (fifteen (pile-sum (cribbage-pile c)))
      (thirty-one (pile-sum (cribbage-pile c)))


    ))


;; ====================================
;; evaluated during DEAL
;; ====================================

;; HIS-HEELS   ** should only run once
;; ------------------------------------------
;; INPUTS: CUT, the card flipped over after the CUT
;; OUTPUTS: 2
;; CONDITION: when the card placed on top of the deck is a JACK of any SUIT

(defun his-heels (cut)
    ;; if CUT == JACK
    (when (equal (rank-of cut) *jack*)
      ;; return value of the scoring opportunity
      2))

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
      1)))


;; GO-SCORE  -- scored retroactively
;; ------------------------------------------
;; INPUTS: PILE-SUM, the sum of the cards placed in the PILE
;;         CURR-PLR-HAND, the current player's hand
;;         NEXT-PLR, the other player
;; OUTPUTS: 1
;; CONDITION: when the next player can't play a card because the PILE-SUM
;;    is already 31 or would go over

(defun go-score (pile curr-plr-hand)
  ;; iterate thru CURR-PLR-HAND
  (dolist (card curr-plr-hand)
    ;; CARD-VALUE + PILE-SUM < 31
    (when (<= (+ (card-value card) (pile-sum pile)) 31)
      (return-from go 0)))
  ;; otherwise have gone through CURR-PLR-HAND and no possibilities
  1)


;; FIFTEEN
;; ------------------------------------------
;; INPUTS: PILE-SUM, the sum of the cards placed in the PILE
;;         SCORE, the score of the last player
;; OUTPUTS: 2
;; CONDITION: when the PILE-SUM reaches 15

(defun fifteen (pile-sum)
    ;; if PILE-SUM == 15
    (when (equal pile-sum 15)
      2))


;; THIRTY-ONE
;; ------------------------------------------
;; INPUTS: PILE-SUM, the sum of the cards placed in the PILE
;;         SCORE, the score of the last player
;; OUTPUTS: 2
;; CONDITION: when the PILE-SUM reaches 31

(defun thirty-one (pile-sum)
  ;; if PILE-SUM == 31
  (when (equal pile-sum 31)
    2))


;; PAIR
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;; OUTPUTS: 2
;; CONDITION: when the previous two cards make a PAIR


;; TRIPLE
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLR, the last player to place a CARD
;; OUTPUTS: 6
;; CONDITION: when the previous three cards make a TRIPLE


;; QUADRUPLE
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLR, the last player to place a CARD
;; OUTPUTS: 12
;; CONDITION: when the previous three cards make a QUADRUPLE


;; RUN
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLR, the last player to place a CARD
;; OUTPUTS: the number of cards in the run
;; CONDITION: the last NUM-CARD's make have continuous
;;    rank (can be out of order)


;; ====================================
;; evaluated during SHOW
;; ====================================

;; FLUSH -- might not in the pile    ************************************
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLR, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds 5)
;; CONDITION: the last 5 cards have the same SUIT
