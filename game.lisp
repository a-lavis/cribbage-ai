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
     (let ((dlr (cribbage-whose-dealer? c))
           (card nil)
           (last-card nil))
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
          (funcall p2-crib-fn c)
          ;; set first CARD selection to P2-PILE-FN
          (setf card (funcall p2-pile-fn c)))
         ((equal dlr *player-two*)
          (format t "DLR to crib.~%")
          (funcall p2-crib-fn c)
          (format t "NON-DLR to crib.~%")
          (funcall p1-crib-fn c)
          ;; set first CARD selection to P1-PILE-FN
          (setf card (funcall p1-pile-fn c))))

       ;; the PLAY, call hand-to-pile! as many times as necessary/possible
       (while (or card last-card)
              (setf last-card card)
              (cond
                ;; call P1-PILE-FN
                ((= (cribbage-whose-turn? c) *player-one*)
                 (setf card (funcall p1-pile-fn c)))
                ;; call P2-PILE-FN
                ((= (cribbage-whose-turn? c) *player-two*)
                 (setf card (funcall p2-pile-fn c)))
                ;; default, assign CARD to NIL to ward off infinite loop
                (t
                  (setf card nil))))

       ;; call SHOW on Cribbage game
       (when (not (game-over? c))
         (show c))))
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

;; PLAY-ONLY-GAME

(defun play-only-game (p1-crib-fn p1-pile-fn p2-crib-fn p2-pile-fn)
  (let ((c (make-cribbage)))
    ;; PLAY-ROUND while NOT GAME-OVER?
    (while (not (game-over? c))
      (play-only-round c p1-crib-fn p1-pile-fn p2-crib-fn p2-pile-fn))
    ;; return who won this game
    (who-won? c)))

;; PLAY-ONLY-ROUND

(defun play-only-round (c p1-crib-fn p1-pile-fn p2-crib-fn p2-pile-fn)
  (cond
    ((not (game-over? c))
     (let ((dlr (cribbage-whose-dealer? c))
           (card nil)
           (last-card nil))
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
          (funcall p2-crib-fn c)
          ;; set first CARD selection to P2-PILE-FN
          (setf card (funcall p2-pile-fn c)))
         ((equal dlr *player-two*)
          (format t "DLR to crib.~%")
          (funcall p2-crib-fn c)
          (format t "NON-DLR to crib.~%")
          (funcall p1-crib-fn c)
          ;; set first CARD selection to P1-PILE-FN
          (setf card (funcall p1-pile-fn c))))

       ;; the PLAY, call hand-to-pile! as many times as necessary/possible
       (while (or card last-card)
              (setf last-card card)
              (cond
                ;; call P1-PILE-FN
                ((= (cribbage-whose-turn? c) *player-one*)
                 (setf card (funcall p1-pile-fn c)))
                ;; call P2-PILE-FN
                ((= (cribbage-whose-turn? c) *player-two*)
                 (setf card (funcall p2-pile-fn c)))
                ;; default, assign CARD to NIL to ward off infinite loop
                (t
                  (setf card nil))))
       ))
    (t
      (format t "Unable to play a round.~%"))))

;; RANDOM-ROUND
;; ------------------------------------------
;; INPUTS: C, a Cribbage game
;; OUTPUTS: none
;; SIDE EFFECTS: plays a round of Cribbage with all random functions

(defun random-round (c)
  (play-round c #'random-to-crib! #'random-to-pile!
	      #'random-to-crib! #'random-to-pile!))


;; RANDOM-DO-PILE
;; ------------------------------------------
;; INPUTS: C, a Cribbage game (after hands have gone to Crib!)
;; OUTPUTS: none

(defun random-do-pile (c)
  ;; check for legal CRIB
  (when (not (= (length (cribbage-crib c)) 4))
    (return-from random-do-pile 'error))
  ;; Put cards in the pile until you can't anymore.
  (let* ((card (random-to-pile! c))
         (last-card card))
    (while (or card last-card)
           (setf last-card card)
           (setf card (random-to-pile! c)))))
         
       

;; RANDOM-GAME
;; ------------------------------------------
;; INPUTS: none
;; OUTPUTS: the winning player

(defun random-game ()
  (play-game #'random-to-crib! #'random-to-pile!
    #'random-to-crib! #'random-to-pile!))


;; PI-MCTS-TO-PILE!

(defun pi-mcts-to-pile! (c)
  (hand-to-pile! c nil (uct-search c 100 2) (cribbage-whose-turn? c)))


;; PR-ROUND

(defun pr-round (c)
  (play-round c #'random-to-crib! #'pi-mcts-to-pile!
              #'random-to-crib! #'random-to-pile!))


;; PI-VS-RANDOM

(defun pi-vs-random ()
  (play-game #'random-to-crib! #'pi-mcts-to-pile!
             #'random-to-crib! #'random-to-pile!))

;; AVG-GAME-WINS

(defun avg-game-wins (times)
  (let ((r (avg-random-wins times))
        (p1 (avg-mcts-p1-wins times))
        (p2 (avg-mcts-p2-wins times)))
    (format t "random: ~A~%p1: ~A~%p2: ~A~%" r p1 p2)))

(defun avg-random-wins (times)
  (let ((x 0))
    (dotimes (i times)
      (incf x (play-only-game #'random-to-crib! #'random-to-pile!
                              #'random-to-crib! #'random-to-pile!)))
    (- x times)))

(defun avg-mcts-p1-wins (times)
  (let ((x 0))
    (dotimes (i times)
      (incf x (play-only-game #'random-to-crib! #'pi-mcts-to-pile!
                              #'random-to-crib! #'random-to-pile!)))
    (- times (- x times))))

(defun avg-mcts-p2-wins (times)
  (let ((x 0))
    (dotimes (i times)
      (incf x (play-only-game #'random-to-crib! #'random-to-pile!
                              #'random-to-crib! #'pi-mcts-to-pile!)))
    (- x times)))
