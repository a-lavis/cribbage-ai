;; FILE: deal.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Contains functions related to the DEAL segment of Cribbage

;; DEAL
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: COUNTER, the counter used to address vector indexes (not used)
;; SIDE EFFECTS: updates the hand vectors in C, ie. plr-one-hand, plr-two-hand

(defun deal (c)
  ;; generate cards for the ROUND
  (let ((cards-dealt (generate-cards))
        (p-one (cribbage-plr-one-hand c))
        (p-two (cribbage-plr-two-hand c))
        (counter 0))
    (format t "dealt: ~A~%" cards-dealt)
    ;; loop thru CARDS-DEALT
    (dolist (card cards-dealt)
      (format t "card: ~A~%" card)
      (cond
        ;; CARDS-DEALT is null, return COUNTER... useless really
        ((null cards-dealt)
          counter)
        ;; assign first five cards to PLR-ONE-HAND
        ((< counter 5)
          ;; set each index of P-ONE's vector
          (setf (cons card p-one)))
        ;; assign second five cards to PLR-TWO-HAND
        ((< counter 10)
          ;; set each index of P-TWO's vector
          (setf (cons card p-two)))
        ;; assign last card to CUT
        (t
          (setf (cribbage-cut c) card)))
      ;; remove FIRST of CARDS-DEALT if NOT NIL
      (when (not (null cards-dealt))
        (remove card cards-dealt))
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
