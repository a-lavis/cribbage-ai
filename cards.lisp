;; FILE: cards.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Explanation of card deck implementation
;;  cards will have a value 0-51

;;  spades will be 0-12
;;  clubs will be 13-25
;;  hearts will be 26-38
;;  diamonds will be 39-51

;; Constants for suits
(defconstant *spades* 0)
(defconstant *clubs* 1)
(defconstant *hearts* 2)
(defconstant *diamonds* 3)
;; the suit vector
(defconstant *suit-vec* #("S" "C" "H" "D"))

;; Constants for special rank
(defconstant *ace* 0)
(defconstant *jack* 10)
(defconstant *queen* 11)
(defconstant *king* 12)
;; the rank vector
(defconstant *rank-vec* #("A" "2" "3" "4" "5" "6" "7" "8" "9" "10"
                          "J" "Q" "K"))

;; MAKE-CARD
;; ------------------------------------------
;; INPUTS: RANK, the rank of a CARD
;;         SUIT, the suit of a CARD
;; OUTPUT: a card value (0-51)

(defun make-card (rank suit)
  (+ rank (* 13 suit)))

;; example: ace-of-spades == (make-card *ace* *spades*)

;; RANK-OF
;; ------------------------------------------
;; INPUTS: CARD, the value (0-51)
;; OUTPUTS: a number referring to the applicable RANK

(defun rank-of (card)
  (mod card 13))


;; SUIT-OF
;; ------------------------------------------
;; INPUTS: CARD, the value (0-51)
;; OUTPUTS: a number referring to the applicable SUIT

(defun suit-of (card)
  (values (floor (/ card 13))))

;; CARD->STRING
;; ------------------------------------------
;; INPUTS: CARD, a number (0-51) referring to a CARD in the deck
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
  (+ 1 (random 52)))


;; CARD-VALUE
;; ------------------------------------------
;; INPUTS: CARD
;; OUTPUTS: numerical value of CARD

(defun card-value (card)
  ;; royal cards are all worth 10
  (if (> (rank-of card) 9)
    10
    (+ 1 (rank-of card))))
