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
  (plr-one-hand '())
  (plr-two-hand '())
  (crib '())
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
  (format str "Crib: ~A~%" (cribbage-crib c))
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


;; PLAY-GAME
;; ------------------------------------------
;;  INPUTS: C, a Cribbage game
;;  OUTPUTS:

(defun play-game (c)
    (while (not (game-over? c))
      (deal c)  ;; completed
      (build-crib c)
      (play c)
      (show c)))


;; GAME-OVER?
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: a Boolean value, T if GAME-OVER

(defun game-over? (c)
  ;; get PLAYERS' scores
  (let ((p-one-score (svref (cribbage-score c) *player-one*))
        (p-two-score (svref (cribbage-score c) *player-two*)))
    ;; GAME OVER if either is >= 61
    (or (>= p-one-score 61) (>= p-two-score 61))))
