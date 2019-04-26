;; FILE: play.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Contains the functions related to the PLAY segment of Cribbage

;; PLAY
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: a pile of cards in order of placement



;; HAND-TO-PILE!
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;;         PILE, the pile of cards
;;         CHECK-LEGAL?, T or NIL
;;         CARD, a card (from one of the players' hands)
;;         PLR, the player (determines whose hand)
;; OUTPUTS: places CARD on PILE
;; SIDE-EFFECTS: removes CARD from player's hand

(defun hand-to-pile! (c pile check-legal? card plr)
  ;; get player's hand
  (let ((plr-hand (svref (cribbage-plr-hands c) plr)))

    ;; check if CARD is legal (ie. in hand)... if CHECK-LEGAL? == T
    (when (and check-legal? (not legal-play? plr-hand pile card))
      ;; print error message
      (format t "Illegal play! Potential issues: crib is full, card is
        not in your hand, you've already passed two cards to the crib.")
      (return-from hand-to-pile! nil))

    ;; remove CARD from PLR-HAND
    (remove card plr-hand)
    ;; add CARD to PILE
    (cons card pile)))
