(defconstant *spades* 0)
(defconstant *clubs* 1)
(defconstant *hearts* 2)
(defconstant *diamonds* 3)
(defconstant *suit-vec* #("♠" "♣" "♥" "♠"))

(defconstant *ace* 1)
(defconstant *jack* 11)
(defconstant *queen* 12)
(defconstant *king* 13)
(defconstant *rank-vec* #("A" "2" "3" "4" "5" "6" "7" "8" "9" "10"
                          "J" "Q" "K"))

(defun make-card (rank suit)
  (+ rank (* 13 suit)))

;;;; example:
;; (defconstant ace-of-spades (make-card *ace* *spades*))

(defun rank-of (card)
  (svref *rank-vec* (mod card 13)))
(defun suit-of (card)
  (svref *suit-vec* (floor (/ card 13))))

(defun card->string (card) ;; add verbose option? "ace of spades"
  (format nil "~A~A" (rank-of card) (suit-of card)))
(defun print-card (card)
  (format t "~A" (card->string card)))


