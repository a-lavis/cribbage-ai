;; FILE: cribbage.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; the PLAYERS
(defparameter *player-one* 0)
(defparameter *player-two* 1)

;;  WIN-LOSS VALUES
(defparameter *win-value* 400000)
(defparameter *loss-value* -400000)

;;  NEGATIVE and POSITIVE INFINITY
(defparameter *neg-inf* -10000000)
(defparameter *pos-inf*  10000000)


;; CRIBBAGE struct
;; ------------------------------------------
;; Fields:
;;    SCORE          -- a vector holding the scores for each player
;;    WHOSE-TURN?    -- either *player-one* or *player-two*
;;    WHOSE-DEALER?  -- either *player-one* or *player-two*
;;    PLR-HANDS      -- a vector holding the hands for each player
;;    CRIB           -- holds the cards placed in the crib after DEAL
;;    PILE           -- holds the cards placed in the pile during PLAY
;;    CUT            -- holds the card that is turned up after deck is cut

(defstruct (cribbage (:print-function print-cribbage))
  (score (vector 0 3))   ;;  PLR-2 pegs "three for last"
  (whose-turn? *player-one*)
  (whose-dealer? *player-one*)
  (plr-hands (vector '() '()))
  (backup-hands (vector '() '()))
  (crib '())
  (pile '())
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
  (format str "Dealer: ~A   " (+ (cribbage-whose-dealer? c) 1))
  (format str "Turn: ~A~%" (+ (cribbage-whose-turn? c) 1))
  (format str "Player-One-Hand: ~A     "
          (mapcar #'card->string (svref (cribbage-plr-hands c) *player-one*)))
  (format str "Player-Two-Hand: ~A~%"
          (mapcar #'card->string (svref (cribbage-plr-hands c) *player-two*)))
  ;; CUT is initialized as NIL, CARD->STRING doesn't like that
  (when (cribbage-cut c)
    (format str "Cut: ~A   " (card->string (cribbage-cut c))))
  (format str "Crib: ~A   " (mapcar #'card->string (cribbage-crib c)))
  (format str "Pile: ~A~%~%" (mapcar #'card->string (cribbage-pile c))))


;; TOGGLE-TURN!
;; ------------------------------------------
;; INPUTS: C, a cribbage struct
;; OUTPUTS: none
;; SIDE-EFFECTS: changes whose turn it is

(defun toggle-turn! (c)
  ;; get the current turn
  (let ((curr-turn (cribbage-whose-turn? c)))
    (setf (cribbage-whose-turn? c) (switch curr-turn))))


;; TOGGLE-DEALER!
;; ------------------------------------------
;; INPUTS: C, a cribbage struct
;; OUTPUTS: none
;; SIDE-EFFECTS: changes who's dealer for the round

(defun toggle-dealer! (c)
  ;; get the current dealer
  (let ((curr-dlr (cribbage-whose-dealer? c)))
    (setf (cribbage-whose-dealer? c) (switch curr-dlr))))


;; SWITCH
;; ------------------------------------------
;; INPUTS: PLR, either *player-one* or *player-two*
;; OUTPUTS: the other player (either *player-one* or *player-two*)

(defun switch (plr)
  (- 1 plr))


;; LEGAL-CRIB?
;; ------------------------------------------
;; INPUTS: PLR-HAND, the player's hand
;;         CRIB, the crib
;;         CARD, the card the player wants to place in the crib
;; OUTPUTS: a Boolean value, T if it's a legal card placement

(defun legal-crib? (plr-hand crib card)
  (and
   ;; PLR-HAND length > 3
   (> (length plr-hand) 3)
   ;; CARD is in PLR-HAND
   (member card plr-hand)
   ;; CRIB length < 4
   (< (length crib) 4)))


;; LEGAL-PLAY?
;; ------------------------------------------
;; INPUTS: PLR-HAND, the player's hand
;;         PILE, the card pile
;;         CARD, the card the player wants to place on the pile
;; OUTPUTS: a Boolean value, T if it's a legal card placement

(defun legal-play? (plr-hand pile card)
  (and
   ;; CARD is in PLR-HAND
   (member card plr-hand)
   ;; sum of PILE would be <= 31
   (<= (+ (card-value card) (pile-sum pile)) 31)))

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


;; WHO-WON?
;; -------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: the name of the player who won, ie. (1 or 2)

(defun who-won? (c)
  ;; return who won when game is over
  (when (game-over? c)
    (let* ((p1-score (svref (cribbage-score c) *player-one*))
       	   (p2-score (svref (cribbage-score c) *player-two*))
       	   (max-score (max p1-score p2-score))
       	   (winning-plr (position max-score (cribbage-score c))))
      (return-from who-won? (+ 1 winning-plr))))
    ;; otherwise, say that game is NOT over
    (format t "Keep playing! It ain't over 'til it's over.~%"))


;; PILE-SUM
;; ------------------------------------------
;; INPUTS: PILE, the card pile
;; OUPUTS: the sum of all the card-values in PILE

(defun pile-sum (pile)
  (apply #'+ (mapcar #'card-value pile)))

;; BACKUP-HANDS
;; ------------------------------------------
(defun backup-hands (c)
  (when (= (length (svref (cribbage-plr-hands c) *player-one*))
           (length (svref (cribbage-plr-hands c) *player-two*))
           3)
    ;; save backup of hands
    (setf (cribbage-backup-hands c) (cribbage-plr-hands c))))


;; MAKE-HASH-KEY-FROM-GAME
;; ------------------------------------------
;; INPUTS: C, a Cribbage game struct
;; OUTPUTS: a list of the form  PLR, P1-HAND, P2-HAND, PILE

(defun make-hash-key-from-game (c)
  (labels ((hand-of (plr)
                    ;; try using (mapcar #'rank-of hand) ???
                    ;; no flush, so only ranks matter
                    (sort (svref (cribbage-plr-hands c) plr) #'<)))
          (list (cribbage-whose-turn? c)
                (hand-of *player-one*)
                (hand-of *player-two*)
                (cribbage-pile c))))


;;  COPY-ARRAY
;; -------------------------------------------------
;;  INPUT:   HARRY, a 2-dimensional array
;;  OUTPUT:  A copy of HARRY

(defun copy-array (harry)
  (let* ((dims (array-dimensions harry))
       	 (kopy (make-array dims)))
    (dotimes (c (first dims))
	(setf (aref kopy c) (aref harry c)))
  kopy))


;; COPY-GAME
;; ------------------------------------------
;; INPUTS: C, a Cribbage game struct
;; OUTPUTS: a copy of the Cribbage game

(defun copy-game (c)
  ;; make the new game struct
  (make-cribbage :score (copy-array (cribbage-score c))
		 :whose-turn? (cribbage-whose-turn? c)
		 :whose-dealer? (cribbage-whose-dealer? c)
		 :plr-hands (copy-array (cribbage-plr-hands c))
		 :backup-hands (copy-array (cribbage-backup-hands c))
		 :crib (cribbage-crib c)
		 :pile (cribbage-pile c)
		 :cut (cribbage-cut c)))
