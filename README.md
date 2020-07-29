# Hearts
This is an implementation of an autonomous player for the card game "Hearts" using Haskell. This is an assignment for FIT2102 Programming Paradigms 2019 S2.

### Hearts introduction
The game of Hearts is a trick-taking game where the goal to score as few points as possible.  All cards in the Hearts are worth one point, and the Queen of Spade is worth 13.  Your task will be to implement a simple AI to beat other players (students and staff) at this game.
The game you will play will follow most of the classic rules of Hearts.  This game is very popular as it is easy to learn and play, thus many online versions are available if you want to train.  Do note, though, that most of them usually vary a little, so take care to read the rules carefully.

At the core of the game of Hearts are the point cards: all cards in Hearts and the Queen of Spade.  The goal of the game is to score as few points as possible.  An alternative exists though: if a player manages to take all point cards, she gets 0 points and every other player gets 26 points (the maximum).

At the end of each round, when players have played all the cards in their hands, we tally the points in every trick taken by each player.  That is, we sum the point cards they won.

The game goes on until at least one player has scored more than 100 points and there is exactly one player with the lowest score.  The player with the lowest score is the winner.

Scoring system
Counting points in Hearts is done as follows:

Point cards: Each Heart is worth 1 point, the Queen of Spades is worth 13 points.
Hand: At the end of each hand (players have played all their cards), each player receives the amount of points in the tricks they won.
Game: The game stops when a player has reached 100 points and exactly one other has the lowest score; the latter is the winner.
Rules of play
The rules of Hearts we will use are as follows:

Reneging: Each turn starts with a player playing a single card, the suit of which determines the suit of the trick.  Other players must follow suit (play cards in the same suit) if they are able to; if they do not have any cards in that suit, they may discard any card of their choosing.
Bleeding: No point card may be played during the first trick unless the player has no other choice.
Leading: The player with the Two of Clubs leads (starts) the first trick with this card.
Breaking: No player may start a trick with a Heart before any has been played, unless they have no other choice.
Shooting the Moon: In case a player took all the point cards, she scores 0 points and the other players all score 26 points.

### Player Strategy

### How to run the code

### Credits
Code base provided by Professor Tim Dwyer and Arthur Maheo.
