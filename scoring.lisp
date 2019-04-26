;; FILE: cards.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Methods in FILE
;; --------------------------
;; SCORING, HIS-HEELS, GO, FIFTEEN, PAIR, TRIPLE, QUADRUPLE, RUN,
;;   FLUSH, HIS-KNOBS

;; using pair to represent players' scores (p1 p2)
;; convention for keeping previous score doesn't seem to have effect on game


;; SCORING
;; ------------------------------------------
;; INPUTS: PLR, the player who last put down a card
;;         PILE, the pile of cards
;; OUTPUTS: the new score for PLR after all possible scoring opportunities
;;       have been considered

(defun pile-score (plr pile)
  ;; get PLR's score
  (let ((plr-score (svref (cribbage-score c) plr)))
    ;; call the scoring fxns
    ))


;; HIS-HEELS
;; ------------------------------------------
;; INPUTS: DEALER-SCORE, the dealer's SCORE
;;         CUT, the card flipped over after the CUT
;; OUTPUTS: the updated dealer's current score value (adds 2)
;; CONDITION: when the card placed on top of the deck is a JACK of any SUIT

(defun his-heels (dealer-score cut)
  ;; get dealer's current SCORE
  (let ((dealer-curr (second dealer-score)))
    ;; if CUT == JACK
    (when (equal (rank-of cut) *jack*)
      ;; add 2 to DEALER-CURR
      (+ dealer-curr 2))))


;; HIS-KNOBS
;; ------------------------------------------
;; INPUTS: TOP-CARD, the top card of the PILE
;;         CUT-SUIT, the SUIT of the card placed after the CUT
;;         LAST-PLAYER, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds 1)
;; CONDITION: the TOP-CARD matches the CUT-SUIT

(defun his-knobs (top-card cut-suit last-player)
  ;; get LAST-PLAYER's current score
  (let ((last-curr (second last-player)))
    ;; if TOP-CARD == CUT-SUIT
    (when (equal (suit-of top-card) cut-suit)
      ;; add 1 to LAST-CURR
      (+ last-curr 1))))


;; GO
;; ------------------------------------------
;; INPUTS: PILE-SUM, the sum of the cards placed in the PILE
;;         LAST-PLAYER, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds 1)
;; CONDITION: when the next player can't play a card because the PILE-SUM
;;    is already 31 or would go over


;; FIFTEEN
;; ------------------------------------------
;; INPUTS: PILE-SUM, the sum of the cards placed in the PILE
;;         LAST-PLAYER, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds 2)
;; CONDITION: when the PILE-SUM reaches 15

(defun fifteen (pile-sum last-player)
  ;; get LAST-PLAYER's current score
  (let ((last-curr (second last-player)))
    ;; if PILE-SUM == 15
    (when (equal pile-sum 15)
      (+ last-curr 2))))


;; PAIR
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLAYER, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds 2)
;; CONDITION: when the previous two cards make a PAIR


;; TRIPLE
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLAYER, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds 6)
;; CONDITION: when the previous three cards make a TRIPLE


;; QUADRUPLE
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLAYER, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds 12)
;; CONDITION: when the previous three cards make a QUADRUPLE


;; RUN
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         NUM-CARDS, the number of cards in the RUN
;;         LAST-PLAYER, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds NUM-CARDS)
;; CONDITION: the last NUM-CARD's make have continuous
;;    rank (can be out of order)




;; FLUSH -- might not in the pile    ************************************
;; ------------------------------------------
;; INPUTS: CARD-PILE, the pile of cards
;;         LAST-PLAYER, the last player to place a CARD
;; OUTPUTS: the updated LAST-PLAYER's score (adds 5)
;; CONDITION: the last 5 cards have the same SUIT
