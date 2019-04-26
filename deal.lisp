;; FILE: deal.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Contains functions related to the DEAL segment of Cribbage

;; DEAL
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: COUNTER, the counter used to decide where to place the card
;; SIDE EFFECTS: updates the hand lists in C, ie. PLR-ONE-HAND, PLR-TWO-HAND

(defun deal (c)
  ;; generate cards for the ROUND
  (let ((cards-dealt (generate-cards))
        (p-one '())    ;; accumulator for plr-one hand
        (p-two '())    ;; accumulator for plr-two hand
        (counter 0))   ;; counter decides where (to whom) the card goes
    ;; loop thru CARDS-DEALT
    (dolist (card cards-dealt)
      (cond
        ;; assign first five cards to PLR-ONE-HAND
        ((< counter 5)
          ;; remove CARD from CARDS-DEALT
          (setf cards-dealt (remove card cards-dealt))
          ;; CONS CARD onto P-ONE
          (setf p-one (cons card p-one)))
        ;; assign second five cards to PLR-TWO-HAND
        ((< counter 10)
          ;; remove CARD from CARDS-DEALT
          (setf cards-dealt (remove card cards-dealt))
          ;; CONS CARD onto P-TWO
          (setf p-two (cons card p-two)))
        ;; assign last card to CUT
        (t
          (setf (cribbage-cut c) card)
          ;; set Cribbage fields to accumulators
          (setf (svref (cribbage-plr-hands c) *player-one*) p-one)
          (setf (svref (cribbage-plr-hands c) *player-two*) p-two)))
      ;; increment COUNTER
      (incf counter))))


;; GENERATE-CARDS
;; ------------------------------------------
;; INPUTS: none
;; OUTPUTS: a list of two lists of cards (each player's hand)

(defun generate-cards ()
  ;; accumulator list for cards dealt
  (labels ((acc-func (card-bucket)
              ;; deal a CARD
              (let ((card (deal-card)))
                (cond
                  ;;  (LENGTH CARD-BUCKET) == 11
                  ((>= (length card-bucket) 11)
                    card-bucket)
                  ;; CARD is not a MEMBER of CARD-BUCKET
                  ((not (member card card-bucket))
                    (acc-func (cons card card-bucket)))
                  ;; CARD is a MEMBER of CARD-BUCKET
                  (t
                    (acc-func card-bucket))))))
      ;; call ACC-FUNC w/ empty CARD-BUCKET
      (acc-func '())))


;; HAND-TO-CRIB!
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;;         CHECK-LEGAL?, T or NIL
;;         CARD, a card (from one of the players' hands)
;;         PLR, the player (determines whose hand)
;; OUTPUTS: none
;; SIDE EFFECTS: takes the specified CARD from PLR-HAND and places into CRIB

(defun hand-to-crib! (c check-legal? card plr)
  ;; get player's hand and the CRIB
  (let ((plr-hand (svref (cribbage-plr-hands c) plr))
        (crib (cribbage-crib c)))

    ;; check if CARD is legal (ie. in hand)... if CHECK-LEGAL? == T
    (when (and check-legal? (not (legal-crib? plr-hand crib card)))
      ;; print error message
      (format t "Illegal play! Potential issues: crib is full, card is
        not in your hand, you've already passed two cards to the crib.")
      (return-from hand-to-crib! nil))

    ;; add CARD to CRIB
    (setf crib (cons card crib))
    ;; remove CARD from PLR-HAND
    (setf plr-hand (remove card plr-hand))
    ;; update CRIBBAGE FIELDS
    (setf (cribbage-crib c) crib)
    (setf (svref (cribbage-plr-hands c) plr) plr-hand)))
