;; FILE: cribbage.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; the PLAYERS
(defconstant *player-one* 0)
(defconstant *player-two* 1)

;;  WIN-LOSS VALUES
(defconstant *win-value* 400000)
(defconstant *loss-value* -400000)

;;  NEGATIVE and POSITIVE INFINITY
(defconstant *neg-inf* -10000000)
(defconstant *pos-inf*  10000000)


;; CRIBBAGE struct
;; ------------------------------------------
;; Fields:
;;    SCORE          -- a vector holding the scores for each player
;;    WHOSE-DEALER?  -- either *player-one* or *player-two*

(defstruct (cribbage (:print-function print-cribbage))
  (score (vector 0 0))
  (whose-dealer? *player-one*)
  (plr-one-hand (vector 0 0 0 0 0))
  (plr-two-hand (vector 0 0 0 0 0))
  cut)


;; PRINT-CRIBBAGE
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: none
;; SIDE EFFECTS: printing the status of the game

(defun print-cribbage (c str depth)
  (declare (ignore depth))
  (format str "Player-One-Score: ~A   " (svref (cribbage-score c) *player-one*))
  (format str "Player-Two-Score: ~A   " (svref (cribbage-score c) *player-two*))
  (format str "Dealer: ~A~%" (cribbage-whose-dealer? c))
  (format str "Player-One-Hand: ~A~%" (cribbage-plr-one-hand c))
  (format str "Player-Two-Hand: ~A~%" (cribbage-plr-two-hand c))
  (format str "Cut: ~A~%" (cribbage-cut c)))


;; TOGGLE-DEALER!
;; ------------------------------------------
;; INPUTS: C, a cribbage struct
;; OUTPUTS: none
;; SIDE-EFFECTS: changes whose turn it is

(defun toggle-dealer! (c)
  ;; get the current dealer
  (let ((curr-dealer (cribbage-whose-dealer? c)))
    (setf (cribbage-whose-dealer? c) (switch curr-dealer))))


;; SWITCH
;; ------------------------------------------
;; INPUTS: DLR, either *player-one* or *player-two*
;; OUTPUTS: the other player (either *player-one* or *player-two*)

(defun switch (dlr)
  (- 1 dlr))


;; PLAY-ROUND
;; ------------------------------------------
;;  INPUTS: C, a Cribbage game
;;  OUTPUTS:

(defun play-round (c)
    (while (not (game-over? c))
      (deal c)
      (play c)
      (show c)))


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
          (setf (svref p-one counter) card))
        ;; assign second five cards to PLR-TWO-HAND
        ((< counter 10)
          ;; set each index of P-TWO's vector
          (setf (svref p-two (- counter 5)) card))
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


;; PLAY
;; ------------------------------------------

;; SHOW
;; ------------------------------------------

;; GAME-OVER?
;; ------------------------------------------
