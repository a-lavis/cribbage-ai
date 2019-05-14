;; FILE: mcts.lisp
;; AUTHORS: Daniel Melody & Aidan Lavis
;; ==========================================

;;  Defines the following functions used by MCTS algorithm
;; ----------------------------------------------------------
;;     GET-ROOT-NODE
;;     NEW-MC-TREE
;;     INSERT-NEW-NODE
;;     SIM-TREE
;;     SIM-DEFAULT
;;     BACKUP
;;     UCT-SEARCH
;;     SELECT-PILE

;;  MC-NODE struct -- a node in the MCTS tree
;; ----------------------------------------------------------------------------
;;  KEY:          a hash-table key (compact rep'n of current state of game)
;;  WHOSE-TURN:   *BLACK* or *WHITE*
;;  NUM-VISITS:   the number of times this state has been visited
;;  VECK-MOVES:   a VECTOR of the legal moves from this state
;;  VECK-VISITS:  a VECTOR recording the number of times each legal move
;;                   has been visited during MCTS
;;  VECK-SCORES:  a VECTOR recording the average scores for the legal
;;                   moves visited during MCTS

(defstruct mc-node
  key
  whose-turn
  (num-visits 0)
  veck-moves
  veck-visits
  veck-scores)

;;  MC-TREE struct -- the MCTS tree
;; -------------------------------------------------------------
;;  HASHY:     a hash-table whose entries are (key,value), where
;;               key = compact repn of state, value = mc-node
;;  ROOT-KEY:  the hash-table key for the root node of the mcts tree

(defstruct mc-tree
  (hashy (make-hash-table :test #'equal))
  root-key)


;;  GET-ROOT-NODE
;; ------------------------------------------
;;  INPUT:   TREE, a MCTS struct
;;  OUTPUT:  The MC-NODE corresponding to the root of the TREE

(defun get-root-node (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))


;;  NEW-MC-TREE
;; ---------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived
;;           from GAME.

(defun new-mc-tree (game)
  (make-mc-tree :root-key (make-hash-key-from-game game)))


;;  INSERT-NEW-NODE
;; ------------------------------------------
;; INPUTS: GAME, a game struct
;;         TREE, an MC-TREE struct
;;         KEY, a hash-key representing the state of the game
;; OUTPUT: newly created and inserted node
;; SIDE EFFECT: inserts a new node into TREE using KEY

(defun insert-new-node (game tree key)
  (let* ((moves (legal-pile game))
         (num-moves (length moves))
         (nodey (make-mc-node
                  :key key
                  :veck-moves moves
                  :veck-visits (make-array num-moves :initial-element 0)
                  :veck-scores (make-array num-moves :initial-element 0)
                  :whose-turn (cribbage-whose-turn? game))))
    ;; insert nodey into tree
    (setf (gethash key (mc-tree-hashy tree)) nodey)
    ;; return the node
    nodey))


;;  SELECT-MOVE
;; ------------------------------------------
;; INPUTS: NODEY, an MC-NODE struct
;;         C, an exploitation-exploration constant
;; OUTPUT: The INDEX of the selected move into the moves vector

(defun select-move (nodey c dlr)
  (let* ((player (mc-node-whose-turn nodey))
         (moves (mc-node-veck-moves nodey))
         (num-moves (length moves)))
    (cond
      ;; No legal moves!
      ((= num-moves 0)
       ;; signal failure
       0)
      ;; Only one legal move
      ((= num-moves 1)
       ;; return it
       0)
      ;; Two or more moves
      (t
        ;; Need to find argmax/argmin of
        ;;   Q(s,a)  +/-  c*sqrt(log(N(s))/N(s,a))
        ;; Note:  Can be called with c=0 or c>0.
        ;;        But if c=0, we can assume n>0 (i.e., *some*
        ;;          node has already been visited)
        (let ((n (mc-node-num-visits nodey))
              (move-visits (mc-node-veck-visits nodey))
              (move-scores (mc-node-veck-scores nodey))
              (best-move-so-far nil)
              (best-score-so-far (if (eq player (switch dlr))
                                   *neg-inf*
                                   *pos-inf*)))
          (dotimes (i num-moves)
            ;; When c>0 and this move has not yet been visited
            ;; Then we want to select it immediately!
            (when (and (> c 0)
                       (= (svref move-visits i) 0))
              (return-from select-move i))
            ;; When c=0 and this move has not yet been visited
            ;; Ignore this move!  (I.e., only proceed if it *has*
            ;; been visited at least once.)
            (when (> (svref move-visits i) 0)
              ;; Fetch average score for this move
              (let ((score (svref move-scores i)))
                ;; When C > 0, update score using UGLY term
                (when (> c 0)
                  (let ((ugly-term (* c (sqrt (/ (log n)
                                                 (svref move-visits i))))))
                    (if (eq player *player-one*)
                      (incf score ugly-term)
                      (decf score ugly-term))))
                ;; When SCORE is better than best-score-so-far...
                (when (or (and (eq player *player-one*)
                               (> score best-score-so-far))
                          (and (eq player *player-two*)
                               (< score best-score-so-far)))
                  ;; Update best-score/move-so-far
                  (setf best-score-so-far score)
                  (setf best-move-so-far i)))))
          ;; Return best-move-so-far or (if NIL) a random move
          (if best-move-so-far
            best-move-so-far
            (random num-moves)))))))


;;  SIM-TREE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is a key into the hashtable, and each move_i
;;    is an index into the MOVES vector of the node assoc with state_i.

(defun sim-tree (game tree c)
  (let (;; KEY-MOVE-ACC:  accumulator of KEYs and MOVEs
        (key-move-acc nil)
        (hashy (mc-tree-hashy tree))
        (curr-move t)
        (last-move t))
    (while (and (not (game-over? game)) (or curr-move last-move))
           (let* (;; KEY:  Hash key for current state of game
                  (key (make-hash-key-from-game game))
                  ;; NODEY:  The MC-NODE corresponding to KEY (or NIL if not in tree)
                  (nodey (gethash key hashy)))
             ;; Case 1:  When key not yet in tree...
             (when (null nodey)
               ;; Create new node and insert it into tree
               (setf nodey (insert-new-node game tree key))
               ;(format t "~%new node: ~A~%" nodey)
               (let* ((mv-index (select-move nodey c (cribbage-whose-dealer? game)))
                      (move-veck (mc-node-veck-moves nodey))
                      (move (svref move-veck mv-index)))
                 (setf last-move curr-move)
                 (setf curr-move move)
                 (funcall #'hand-to-pile! game nil move (cribbage-whose-turn? game))
                 (push key key-move-acc)
                 (push mv-index key-move-acc)
                 ;; return the accumulator prepended with selected MOVE
                 ;; and KEY for current state
                 (return-from sim-tree (reverse key-move-acc))))

             ;; Case 2:  Key already in tree!
             ;(format t "~%old node: ~A~%" nodey)
             (let* ((mv-index (select-move nodey c (cribbage-whose-dealer? game)))
                    (move-veck (mc-node-veck-moves nodey))
                    (move (svref move-veck mv-index)))
               (setf last-move curr-move)
               (setf curr-move move)
               (funcall #'hand-to-pile! game nil move (cribbage-whose-turn? game))
               (push key key-move-acc)
               (push mv-index key-move-acc))))

    ;; After the WHILE... return the accumulated key/move list
    (reverse key-move-acc)))


;;  SIM-DEFAULT
;; ----------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  an evaluation of the win/loss record of the round played
;;  NOTE: GAME must have a full CRIB for RANDOM-DO-PILE to work
;;    we are only exploring the PLAY portion of the game with this
;;    MCTS implementation

(defun sim-default (game)
    ;; play out the round
    (random-do-pile game)
    ;; get players' scores
    (labels ((score (plr) (svref (cribbage-score game) plr)))
      (- (score *player-one*) (score *player-two*))))

;;  BACKUP
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY

(defun backup (hashy key-move-acc result)
  (while key-move-acc
         (let* ((key (pop key-move-acc))
                (nodey (gethash key hashy))
                (mv-index (pop key-move-acc))
                (visitz (mc-node-veck-visits nodey))
                (scorez (mc-node-veck-scores nodey)))
           ;; increment node num visits
           (incf (mc-node-num-visits nodey))
           ;; increment num times did this move from this state
           (incf (svref visitz mv-index))
           ;; increment the SCORE
           (incf (svref scorez mv-index)
                 (/ (- result (svref scorez mv-index))
                    (svref visitz mv-index))))))


;;  UCT-SEARCH
;; ---------------------------------
;;  INPUTS:  ORIG-GAME, a game struct w/ fully constructed CRIB
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of MCTS.

(defparameter *verbose* t) ;; a global parameter used to ensure/suppress printing of stats

(defun uct-search (orig-game num-sims c)
  ;; Want to use COPY of GAME struct for simulations...
  ;; That way, can reset game struct before each simulation...
  (let* ((tree (new-mc-tree orig-game))
         (hashy (mc-tree-hashy tree))
	 (dlr (cribbage-whose-dealer? orig-game))
	 ;;(player (whose-turn orig-game))
         )
    (dotimes (i num-sims)
      (format t "~A, " i)
      ;;(format t " NEW SIM")
      (let* (;; Work with a COPY of the original game struct
             (game (copy-game orig-game))
	     ;; Phase 1:  SIM-TREE Destructively modifies game
             (key-move-acc (sim-tree game tree c))
	     ;; Phase 2:  SIM-DEFAULT returns result
             (playout-result (sim-default game)))
	;; Finally, backup the results
        (backup hashy key-move-acc playout-result)
        ))
    ;; Select the best move (using c = 0 because we are not exploring anymore)
    (let* ((rootie (get-root-node tree))
           (mv-index (select-move rootie 0 dlr))
           (move (svref (mc-node-veck-moves rootie) mv-index))
           (scores (mc-node-veck-scores rootie))
           (score (svref scores mv-index)))
      (format t ".")
      (when *verbose*
        ;; Display some stats along with the best move
        (format t "Best score: ~5,3F score veck: " score)
        (dotimes (i (length scores))
          (format t "~5,3F, " (svref scores i)))
        (format t "~%")
        (format t "Visits veck: ")
        (dotimes (i (length scores))
          (format t "~A " (svref (mc-node-veck-visits rootie) i)))
        (format t "~%"))
      ;; Output the move
      (format t "move: ~A~%" move)
      move)))
