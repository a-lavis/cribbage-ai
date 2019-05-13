;; FILE: game.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; functions relating to the actual play of Cribbage

;; PLAY-ROUND
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;;         P1-CRIB-FN, a fxn for deciding cards given to CRIB for player 1
;;         P1-PILE-FN, a fxn for deciding cards given to PILE for player 1
;;         P2-CRIB-FN, a fxn for deciding cards given to CRIB for player 2
;;         P2-PILE-FN, a fxn for deciding cards given to PILE for player 2
;; OUTPUTS: none
;; SIDE EFFECTS: plays a round of Cribbage and updates Cribbage struct

(defun play-round (c p1-crib-fn p1-pile-fn p2-crib-fn p2-pile-fn)
  (cond
    ((not (game-over? c))
      (let ((p1-hand (svref (cribbage-plr-hands c) 0))
            (p2-hand (svref (cribbage-plr-hands c) 1))
            (dlr (cribbage-whose-dealer? c))
	          (pile (cribbage-pile c))
            (p1-legals '())
            (p2-legals '()))
        ;; set turn to DLR
        (setf (cribbage-whose-turn? c) dlr)
         ;; DEAL the cards
	       (format t "DEAL~%")
	       (deal c)
         ;; give cards to CRIB
         (cond
           ;; DLR = Player One
           ((equal dlr *player-one*)
             (format t "DLR to crib.~%")
             (funcall p1-crib-fn c)
             (format t "NON-DLR to crib.~%")
	           (funcall p2-crib-fn c))
           ((equal dlr *player-two*)
             (format t "DLR to crib.~%")
             (funcall p2-crib-fn c)
             (format t "NON-DLR to crib.~%")
	           (funcall p1-crib-fn c)))

         ;; the PLAY, call hand-to-pile! as many times as necessary/possible
	       (while (or (member t (dolist (card p1-hand p1-legals)
		                  (push (legal-play? p1-hand pile card) p1-legals)))
		                (member t (dolist (card p2-hand p2-legals)
		                  (push (legal-play? p2-hand pile card) p2-legals))))
	         (format t "Player ~A to pile.~%" (1+ (cribbage-whose-turn? c)))
	         ;; clear P1-LEGALS and P2-LEGALS
	         (setf p1-legals '())
	         (setf p2-legals '())
	         ;; call correct hand-to-pile! func
	         (if (equal (cribbage-whose-turn? c) *player-one*)
		         (funcall p1-pile-fn c)
	           (funcall p2-pile-fn c)))

        ;; call SHOW on Cribbage game
        (show c)))
    (t
     (format t "Unable to play a round.~%"))))


;; PLAY-GAME
;; ------------------------------------------
;; INPUTS: P1-CRIB-FN, a function governing hand-to-crib! choices for player 1
;;         P1-PILE-FN, a function governing hand-to-pile! choices for player 1
;;         P2-CRIB-FN, a function governing hand-to-crib! choices for player 2
;;         P2-PILE-FN, a function governing hand-to-pile! choices for player 2
;; OUTPUTS: the winning player

(defun play-game (p1-crib-fn p1-pile-fn p2-crib-fn p2-pile-fn)
  (let ((c (make-cribbage)))
    ;; PLAY-ROUND while NOT GAME-OVER?
    (while (not (game-over? c))
      (play-round c p1-crib-fn p1-pile-fn p2-crib-fn p2-pile-fn))
    ;; return who won this game
    (who-won? c)))
