;; Constants for suits
(defconstant *spades* 0)
(defconstant *clubs* 1)
(defconstant *hearts* 2)
(defconstant *diamonds* 3)
;; the suit vector
(defconstant *suit-vec* #("♠" "♣" "♥" "♠"))

;; Constants for special rank
(defconstant *ace* 1)
(defconstant *jack* 11)
(defconstant *queen* 12)
(defconstant *king* 13)
;; the rank vector
(defconstant *rank-vec* #("A" "2" "3" "4" "5" "6" "7" "8" "9" "10"
                          "J" "Q" "K"))

;; MAKE-CARD
;; ------------------------------------------
;; INPUTS: RANK, the rank of a CARD
;;         SUIT, the suit of a CARD
;; OUTPUT: a card value (1-52)

(defun make-card (rank suit)
  (+ rank (* 13 suit)))

;;;; example:
;; (defconstant ace-of-spades (make-card *ace* *spades*))

;; RANK-OF
;; ------------------------------------------
;; INPUTS: CARD, the value (1-52)
;; OUTPUTS: a number referring to the applicable RANK

(defun rank-of (card)
  (mod card 13))


;; SUIT-OF
;; ------------------------------------------
;; INPUTS: CARD, the value (1-52)
;; OUTPUTS: a number referring to the applicable SUIT

(defun suit-of (card)
  (floor (/ card 13)))

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
;; OUTUTS: NIL (doesn't matter)
;; SIDE EFFECTS: prints out the CARD->STRING value
(defun print-card (card)
  (format t "~A" (card->string card)))
