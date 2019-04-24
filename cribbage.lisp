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
  (whose-dealer? *player-one*))


;; PRINT-CRIBBAGE
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: none
;; SIDE EFFECTS: printing the status of the game

(defun print-cribbage (c str depth)
  (declare (ignore depth))
  (format t "Player-One-Score: ~A   " (svref (cribbage-score c) *player-one*))
  (format t "Player-Two-Score: ~A   " (svref (cribbage-score c) *player-two*))
  (format t "Dealer: ~A~%" (cribbage-whose-dealer? c)))


;; TOGGLE-DEALER!
;; ------------------------------------------
;; INPUTS: C, a cribbage struct
;; OUTPUTS: none
;; SIDE-EFFECTS: changes whose turn it is

(defun toggle-turn! (c)
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

;; PLAY
;; ------------------------------------------

;; SHOW
;; ------------------------------------------

;; GAME-OVER?
;; ------------------------------------------
