;; FILE: cards.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Explanation of card deck implementation
;;  cards will have a value 1-52

;;  spades will be 1-13
;;  clubs will be 14-26
;;  hearts will be 27-39
;;  diamonds will be 40-52

;; Constants for suits
(defparameter *spades* 0)
(defparameter *clubs* 1)
(defparameter *hearts* 2)
(defparameter *diamonds* 3)
;; the suit vector
(defparameter *suit-vec* #("S" "C" "H" "D"))

;; Constants for special rank
(defparameter *ace* 1)
(defparameter *jack* 11)
(defparameter *queen* 12)
(defparameter *king* 13)
;; the rank vector
(defparameter *rank-vec* #("error" "A" "2" "3" "4" "5" "6" "7" "8" "9" "10"
                          "J" "Q" "K"))

;; MAKE-CARD
;; ------------------------------------------
;; INPUTS: RANK, the rank of a CARD
;;         SUIT, the suit of a CARD
;; OUTPUT: a card value (1-52)

(defun make-card (rank suit)
  (+ rank (* 13 suit)))

;; example: ace-of-spades == (make-card *ace* *spades*)

;; RANK-OF
;; ------------------------------------------
;; INPUTS: CARD, the value (1-52)
;; OUTPUTS: a number referring to the applicable RANK

(defun rank-of (card)
  (1+ (mod (1- card) 13)))


;; SUIT-OF
;; ------------------------------------------
;; INPUTS: CARD, the value (1-52)
;; OUTPUTS: a number referring to the applicable SUIT

(defun suit-of (card)
  (values (floor (/ (1- card) 13))))

;; CARD->STRING
;; ------------------------------------------
;; INPUTS: CARD, a number (1-52) referring to a CARD in the deck
;; OUTPUTS: a string illustrating that CARD

(defun card->string (card) ;; add verbose option? "ace of spades"
  (format nil "~A~A" (svref *rank-vec* (rank-of card))
                     (svref *suit-vec* (suit-of card))))

;; PRINT-CARD
;; ------------------------------------------
;; INPUTS: CARD, a number (1-52) referring to a CARD in the deck
;; OUTPUTS: NIL (doesn't matter)
;; SIDE EFFECTS: prints out the CARD->STRING value
(defun print-card (card)
  (format t "~A  " (card->string card)))


;; DEAL-CARD
;; ------------------------------------------
;; INPUTS: none
;; OUTPUTS: a random card number

(defun deal-card ()
  (1+ (random 52)))


;; CARD-VALUE
;; ------------------------------------------
;; INPUTS: CARD
;; OUTPUTS: numerical value of CARD

(defun card-value (card)
  ;; royal cards are all worth 10
  (min (rank-of card) 10))
