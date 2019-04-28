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
  (let ((plr-hand (svref (cribbage-plr-hands c) plr))
        (pile (cribbage-pile c)))

    ;; check if CARD is legal (ie. in hand)... if CHECK-LEGAL? == T
    (when (and check-legal? (not (legal-play? plr-hand pile card)))
      ;; print error message
      (format t "Illegal play! Potential issues: crib is full, card is
        not in your hand, you've already passed two cards to the crib.")
      (return-from hand-to-pile! nil))

    ;; remove CARD from PLR-HAND
    (setf plr-hand (remove card plr-hand))
    ;; add CARD to PILE
    (setf pile (cons card pile))
    ;; update CRIBBAGE fields
    (setf (svref (cribbage-plr-hands c) plr) plr-hand)
    (setf (cribbage-pile c) pile)
    (setf (cribbage-whose-dealer? c) (toggle-dealer! c))

    ;; call scoring fxn
    ))


;; PILE-SUM
;; ------------------------------------------
;; INPUTS: PILE, the card pile
;; OUPUTS: the sum of all the card-values in PILE

(defun pile-sum (pile)
  (apply #'+ (mapcar #'card-value pile)))
