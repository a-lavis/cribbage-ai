;; FILE: game.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; functions relating to the actual play of Cribbage

;; PLAY-ROUND
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;;         TO-CRIB, a fxn for deciding cards given to CRIB
;;         TO-PILE, a fxn for deciding cards given to PILE

(defun play-round (c crib-fn pile-fn)
  (cond
    ((not (game-over? c))
      (let ((p1-hand (svref (cribbage-plr-hands c) 0))
            (p2-hand (svref (cribbage-plr-hands c) 1))
            (pile (cribbage-pile c)))
        ;; DEALER's hand-to-crib!
        (funcall crib-fn c)
        ;; NON-DEALER's hand-to-crib!
        (funcall crib-fn c)
        ;; call hand-to-pile! as many times as necessary/possible
        (while (or (dolist (card p1-hand)
		     (legal-play? p1-hand pile card))
		   (dolist (card p2-hand)
		     (legal-play? p2-hand pile card)))
          (funcall pile-fn c))))
    (t
      (format t "Unable to play a round.~%"))))
