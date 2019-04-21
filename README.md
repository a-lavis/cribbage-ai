# CMPU365 Final Project Authors: `Daniel Melody`, `Aidan Lavis`

## The Project

For this project, we have decided to implement a 2-player, 5-card version of
cribbage playing to 61.  This is the simplest version of cribbage and is
preferred by beginners.  There are 6-card versions, as well as 3-player and
4-player versions.

We have refactored the Expecti-minimax, Expecti-minimax w/ pruning, as well as
Monte Carlo Tree Search algorithms to work with our implementation of Cribbage.
We will compare the performances of these various chance-based adversarial
search algorithms.

## The Game

Cribbage requires a 52-card standard playing deck.  The current and previous
scores of each player are tracked by a board which counts up to 121 for full
games, but this simplified version will terminate after 61 points. The royal
card values are all worth 10 points.  Aces count as 1 point, though.  

Cribbage is divided into three segments: Deal, Play, and Show.

The Deal happens at the beginning of each round.  Typically, the players will
draw for lowest card to determine who begins as the dealer.  We will forego this
process and just designate one player as the dealer.Then each player is dealt their
hand (5 cards).  From these five cards, each player selects

The Play happens each round w/ a new hand.  This segment starts w/ the
non-dealer player (the pone) laying a card down.  The dealer follows, announcing
the cumulative amount.  This continues until the value reaches 31, or a player
can't place a card down without going over 31.  Then a new card pile is started.
Same rules apply until the players' hands are empty.  There are many
opportunities to score points during the Play (these will be covered in the
Scoring Opportunities section).

The Show starts after the Play has finished.  The players take back their cards
and calculate all the different card combinations (covered in Scoring
Opportunities).

### Scoring Opportunities

Rules from https://cardgames.io/cribbage/#rules
