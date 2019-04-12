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
;; OUTPUTS: a string containing the applicable RANK

(defun rank-of (card)
  (svref *rank-vec* (mod card 13)))


;; SUIT-OF
;; ------------------------------------------
;; INPUTS: CARD, the value (1-52)
;; OUTPUTS: a string containing the applicable SUIT

(defun suit-of (card)
  (svref *suit-vec* (floor (/ card 13))))


(defun card->string (card) ;; add verbose option? "ace of spades"
  (format nil "~A~A" (rank-of card) (suit-of card)))


(defun print-card (card)
  (format t "~A" (card->string card)))
