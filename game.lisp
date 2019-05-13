;; FILE: game.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; functions relating to the actual play of Cribbage

;; PLAY-ROUND
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;;         TO-CRIB, a fxn for deciding cards given to CRIB
;;         TO-PILE, a fxn for deciding cards given to PILE

(defun play-round (c to-crib to-pile)
  (cond
    ((not (game-over? c))
      (let ((p1-hand (svref (cribbage-plr-hands c) 0))
            (p2-hand (svref (cribbage-plr-hands c) 1))
            (pile (cribbage-pile c)))
        ;; DEALER's hand-to-crib!
        (apply #'to-crib c)
        ;; NON-DEALER's hand-to-crib!
        (apply #'to-crib c)
        ;; call hand-to-pile! as many times as necessary/possible
        (while (or (legal-pile? p1-hand *CARD*)
                  (legal-pile! p2-hand #CARD*))
          (apply #'to-pile c))))
    (t
      (format t "Unable to play a round.~%"))))
