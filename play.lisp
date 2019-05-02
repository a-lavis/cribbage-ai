;; FILE: play.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Contains the functions related to the PLAY segment of Cribbage

;; HAND-TO-PILE!
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;;         CHECK-LEGAL?, T or NIL
;;         CARD, a card (from one of the players' hands)
;;         PLR, the player (determines whose hand)
;; OUTPUTS: places CARD on PILE
;; SIDE-EFFECTS: removes CARD from player's hand

(defun hand-to-pile! (c check-legal? card plr)
  ;; get player's hand
  (let ((plr-hand (svref (cribbage-plr-hands c) plr)))

    ;; check if CARD is legal (ie. in hand)... if CHECK-LEGAL? == T
    (when (and check-legal? (not (legal-play? plr-hand (cribbage-pile c) card)))
      ;; print error message
      (format t "Illegal play! Potential issues: pile will go over 31 with
        any of your remaining cards, no cards left, not a card.")
      (return-from hand-to-pile! nil))

    ;; remove CARD from PLR-HAND
    (setf plr-hand (remove card plr-hand))
    ;; add CARD to PILE
    (setf (cribbage-pile c) (cons card pile))
    ;; update CRIBBAGE-PLR-HANDS
    (setf (svref (cribbage-plr-hands c) plr) plr-hand)
    ;; call PILE-SCORE
    (setf (svref (cribbage-score c) plr) (pile-score c))
    ;; change WHOSE-TURN?
    (setf (cribbage-whose-turn? c) (toggle-turn! c))))


;; PILE-SUM
;; ------------------------------------------
;; INPUTS: PILE, the card pile
;; OUPUTS: the sum of all the card-values in PILE

(defun pile-sum (pile)
  (apply #'+ (mapcar #'card-value pile)))
