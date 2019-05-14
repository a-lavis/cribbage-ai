# CMPU365 Final Project

*Authors*: Daniel Melody, Aidan Lavis

## The Project

For this project, we have decided to implement a 2-player, 5-card version of
Cribbage playing to 61.  This is the simplest version of Cribbage and is
preferred by beginners.  There are 6-card versions, as well as 3-player and
4-player versions.

We have refactored the Monte Carlo Tree Search algorithm to work with our
implementation of Cribbage. We will compare the performance of a perfect
information version, an imperfect information version, and a random player.

## The Game

Cribbage requires a 52-card standard playing deck.  The current and previous
scores of each player are tracked by a board which counts up to 121 for full
games, but this simplified version will terminate after 61 points. The royal
card values are all worth 10 points.  Aces count as 1 point.  

Cribbage is divided into three segments: Deal, Play, and Show.

The Deal happens at the beginning of each round.  Typically, the players will
draw for lowest card to determine who begins as the dealer.  We will forego this
process and just designate one player as the dealer.Then each player is dealt
their hand (5 cards).  From these five cards, each player selects

The Play happens each round w/ a new hand.  This segment starts w/ the
non-dealer player (the pone) laying a card down.  The dealer follows, announcing
the cumulative amount.  This continues until the value reaches 31, or a player
can't place a card down without going over 31.  Then a new card pile is started.
Same rules apply until the players' hands are empty.  There are many
opportunities to score points during the Play (these will be covered in the
Scoring Opportunities section).

The Show starts after the Play has finished.  The players take back their cards
and calculate all the different card combinations (covered in Scoring
Opportunities).  It is important to note that cards can be reused when creating
scoring combos.  First, the non-dealer's hand is evaluated, next the dealer's,
and finally the crib.  The points scored by the crib are given to the dealer.
This creates an advantage for the first dealer (made even larger by our shorter
game).

### Scoring Opportunities

Rules from https://cardgames.io/Cribbage/#rules

### Using Our Game Implementation

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

Note: we have specialized round and game functions which wrap the above
functions.

For example, ```(pr-round c)``` wraps the function call ```(play-round
#'random-to-pile! #'pi-mcts-to-pile! #'random-to-crib! #'random-to-pile!)```

### The Process

## Implementing Cribbage


## Adapting MCTS for Cribbage
