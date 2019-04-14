;; FILE: cards.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; a beginning attempt to sketch out the different scoring fxns

;; Methods in FILE
;; --------------------------
;; HIS-HEELS, GO, FIFTEEN, PAIR, TRIPLE, QUADRUPLE, RUN, FLUSH, HIS-KNOBS

;; for now using a pair to represent previous score and current score


;; HIS-HEELS
;; ------------------------------------------
;; INPUTS: DEALER-SCORE, the representation of a dealer's SCORE
;;         CUT, the card flipped over after the CUT
;; OUTPUTS: the updated dealer's score value

(defun his-heels (dealer-score cut)
  ;; get dealer's current SCORE
  (let ((curr-score (second dealer-score))))
    ;; if CUT == any JACK
    (when (or (equal cut (* *jack* *spades*))
              (equal cut (* *jack* *clubs*))
              (equal cut (* *jack* *hearts*))
              (equal cut (* *jack* *diamonds*)))
      ;; add 1 to DEALER-SCORE
      (incf dealer-score)))
