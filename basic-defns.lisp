;; FILE: basic-defns.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;; Ensure efficient tail-recursion handling
(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t)

;; avoid garbage-collection messages
(setf *global-gc-behavior* :auto)

;; list of files for CRIBBAGE implementation:
(defparameter *cribbage-files*
  (list "basic-defns"
        "cards"
        "cribbage"
        "deal"
        "play"
        "show"
	"game"
        ))


;; MAKER
;; ------------------------------------------
;; Compiles and loads all files for Cribbage implementation

(defun maker ()
  (dolist (file *cribbage-files*)
    (compile-file file)
    (load file)))
