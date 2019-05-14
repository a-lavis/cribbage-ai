# CMPU365 Final Project

*Authors*: Daniel Melody, Aidan Lavis

## Using Our Game Implementation

Option 0: Specialized game play
  $ (pi-vs-random)
    - wraps the next option
    - calls MCTS as the p1-pile-fn with all other input functions as random
  $ (pr-round c)
    - takes in a game instance and operates MCTS for the PLAY portion of a
    single round

Option 0.5: The preferred option
  $ (avg-game-wins times)
    - plays number of TIMES against a random-player and returns the results

Option 1: Straight-up game play
  $ (play-game p1-crib-fn p1-pile-fn p2-crib-fn p2-pile-fn)  
    - because this game has two separate decision points, we need to give
    the game-playing automation instruction for both players, and both decision
    points
    - example functions: random-to-crib!, random-to-pile!, select-to-crib!,
    select-to-pile!
      - where select* functions use a search algorithm to generate an informed
      choice
    - this enables us to compare algorithms in head-to-head play

Option 2: Play by the round
  $ (play-round p1-crib-fn p1-pile-fn p2-crib-fn p2-pile-fn)
    - same as above, except it only runs one round rather than playing until
    end of game

Option 3: Manual game play (functions explained in order of calling)
  $ (setf c (make-cribbage))
    - make the Cribbage game struct
  $ (deal c)
    - randomly deal out cards to the two players and the CUT card
  $ (hand-to-crib! c check-legal? card1 card2 plr)
    - select 2 cards from PLR's hand to give to the CRIB
    - set CHECK-LEGAL? to T if not sure of move-legality
    - happens once per player (DEALER goes first)
  $ (hand-to-pile! c check-legal? card plr)
    - same kind of thing as hand-to-crib!
    - call this until no legal moves left (hands run out, pile can't stay below
    31)
    - this tracks scoring opportunities for the PLAY and updates Cribbage game
    struct as each move
  $ (show c)
    - calls the scoring functions for the SHOW
    - NON-DEALER's hand is evaluated first, then DEALER, then CRIB
      - CRIB scoring is added to DEALER's points for the round
