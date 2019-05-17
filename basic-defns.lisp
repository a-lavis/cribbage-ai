;; FILE: basic-defns.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Use SBCL terminal environment and it works

;; list of files for CRIBBAGE implementation:
(defparameter *cribbage-files*
  (list "basic-defns.fasl"
        "cards.fasl"
        "cribbage.fasl"
        "deal.fasl"
        "play.fasl"
        "show.fasl"
        "mcts-perfect.fasl"
        "game.fasl"))


;; MAKER
;; ------------------------------------------
;; Loads all FASL files for Cribbage implementation

(defun maker ()
  (dolist (file *cribbage-files*)
          (format t "getting here ~A~%" file)
          (load file)))
