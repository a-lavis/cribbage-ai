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
  (let ((cards-dealt (generate-cards)))
    ;; assign first five cards to PLR-ONE-HAND
    (setf (svref (cribbage-plr-hands c) *player-one*)
          (subseq cards-dealt 0 5))
    ;; assign second five cards to PLR-TWO-HAND
    (setf (svref (cribbage-plr-hands c) *player-two*)
          (subseq cards-dealt 5 10))
    ;; assign last card to CUT
    (setf (cribbage-cut c) (first (last cards-dealt))))
  ;; call HIS-HEELS and update CRIBBAGE-SCORE
  (incf (svref (cribbage-score c) (cribbage-whose-dealer? c))
        (his-heels (cribbage-cut c)))
  ;; print out updated C
  (print-cribbage c t 1))


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
                  ;;  we have 11 CARDS
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
;;         CARD1, a card (from one of the players' hands)
;;         CARD2, a card
;;         PLR, the player (determines whose hand)
;; OUTPUTS: none
;; SIDE EFFECTS: takes the specified CARD from PLR-HAND and places into CRIB

(defun hand-to-crib! (c check-legal? card1 card2 plr)
  ;; get player's hand and the CRIB
  (let ((plr-hand (svref (cribbage-plr-hands c) plr))
        (crib (cribbage-crib c)))
    ;; check if cards legal (ie. in hand)... if CHECK-LEGAL? == T
    (when (and check-legal? (not (legal-crib? plr-hand crib card1))
                            (not (legal-crib? plr-hand crib card2))
                            (not (equal card1 card2)))
      ;; print error message
      (format t "Illegal play! Potential issues: crib is full, card is
        not in your hand, you've already passed two cards to the crib.")
      (return-from hand-to-crib! nil))
    ;; add CARD to CRIB
    (setf crib (append (list card1 card2) crib))
    ;; remove CARDS from PLR-HAND
    (setf plr-hand (remove card1 plr-hand))
    (setf plr-hand (remove card2 plr-hand))
    ;; update CRIBBAGE FIELDS
    (setf (cribbage-crib c) crib)
    ;; change WHOSE-TURN? when CRIB length != 4
    (when (not (= (length crib) 4)) (toggle-turn! c))
    (setf (svref (cribbage-plr-hands c) plr) plr-hand))
  ;; print Cribbage struct
  (print-cribbage c t 1))


;; RANDOM-CRIB
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: one of the legal moves available to current game

(defun random-crib (c)
  (let* ((card1 nil)
         (card2 nil)
         (plr (cribbage-whose-turn? c))
         (plr-hand (svref (cribbage-plr-hands c) plr)))
    ;; get random NTH CARD2 from PLR-HAND
    (setf card1 (nth (random (length plr-hand)) plr-hand))
    ;; remove CARD1 from PLR-HAND
    (setf plr-hand (remove card1 plr-hand))
    ;; get random NTH CARD2 from PLR-HAND
    (setf card2 (nth (random (length plr-hand)) plr-hand))
    ;; return list of CARD1, CARD2
    (list card1 card2)))


;; RANDOM-TO-CRIB!
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: one of the available legal moves

(defun random-to-crib! (c)
  (let* ((crib (random-crib c)))
    (format t "1-crib: ~A~%" (first crib))
    (format t "2-crib: ~A~%" (second crib))
    (hand-to-crib! c nil (first crib) (second crib)
      (cribbage-whose-turn? c))))


;; HIS-HEELS
;; ------------------------------------------
;; INPUTS: CUT, the card flipped over after the CUT
;; OUTPUTS: 2
;; CONDITION: when the card placed on top of the deck is a JACK of any SUIT

(defun his-heels (cut)
  ;; if CUT == JACK, return 2, else 0
  (if (equal (rank-of cut) *jack*) 2 0))
