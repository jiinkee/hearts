# FIT2102 2019 Assignment 2: Functional Programming in Haskell
Task Description: 
You will create an autonomous player for the card game **Hearts**. You will do this by filling in the undefined playCard function in the Player.hs file that you receive in the code bundle. 

### The Game
The game of [Hearts] (https://en.wikipedia.org/wiki/Hearts_(card_game)) is a trick-taking game where the goal to score as *few points* as possible.  All cards in the Hearts are worth one point, and the Queen of Spade is worth 13.  Your task will be to implement a simple AI to beat other players (students and staff) at this game.

There will be two versions of the game:
1. A two-player version, which will be used to assess the *strength* of your player.  This version will use calibrated opponents and can be seen as a test.
2. An online tournament where your player will be pitted against other students’ AIs in four-player matches.

To play the game with your AI, you will need to implement a function called `playFunc` which will be given a number of parameters upon which you will need to act and return the card you believe will lead to victory.  An important component of the game setup is that you will be allowed to have a form of *memory*.

Memory, in this case, will be entirely determined by you.  The game will simply pass you information pertaining to the current trick: your hand; cards played by players before you; and the previous trick: the complete set of cards played at the previous trick and your memory.

The game you will play will follow most of the classic rules of Hearts.  This game is very popular as it is easy to learn and play, thus many online versions are available if you want to train.  Do note, though, that most of them usually vary a little, so take care to read the rules carefully.

At the core of the game of Hearts are the *point cards*: all cards in Hearts and the Queen of Spade.  The goal of the game is to score as few points as possible.  An alternative exists though: if a player manages to take all point cards, she gets 0 points and *every other player* gets 26 points (the maximum).

At the end of each *round*, when players have played all the cards in their hands, we tally the points in every trick taken by each player.  That is, we sum the point cards they won.

The game goes on until at least one player has scored more than 100 points *and* there is exactly one player with the lowest score.  The player with the lowest score is the winner.

### Scoring system
Counting points in Hearts is done as follows:
- **Point cards**: Each Heart is worth 1 point, the Queen of Spades is worth 13 points.
- **Hand**: At the end of each hand (players have played all their cards), each player receives the amount of points in the tricks they won.
- **Game**: The game stops when a player has reached 100 points and exactly one other has the lowest score; the latter is the winner.

### Rules of play
The rules of Hearts we will use are as follows:
- **Reneging**: Each turn starts with a player playing a single card, the suit of which determines the suit of the trick.  Other players must follow suit (play cards in the same suit) if they are able to; if they do not have any cards in that suit, they may discard any card of their choosing.
- **Bleeding**: No point card may be played during the first trick unless the player has no other choice.
- **Leading**: The player with the Two of Clubs leads (starts) the first trick with this card.
- **Breaking**: No player may start a trick with a Heart before any has been played, unless they have no other choice.
- **Shooting the Moon**: In case a player took all the point cards, she scores 0 points and the other players all score 26 points.

# Deliverable
Your task is to write a Player for Hearts which will implement the following function:
`playCard :: PlayFunc -- play a card from your hand during a trick`

You will have two tasks during the assignment period:
1. Upload your player to [the tournament] (https://fit2102.monash/uploader/) so you can evaluate your player's performance.
2. Upload your Player.hs file to Moodle before Friday 18th October 11:55pm.

Before uploading your player, please check that the following run:
`$ stack test`
This will run the tests on your player, making sure your functions respect the playing rules. If your code does not pass the tests, you will not be able to access the tournament. 

The code provided uses the Safe pragma to make sure the code you use is okay to run. It is also compiled with the -Werror flag which means that all warnings will throw errors and prevent your code from compiling. So do make sure you run the test suite before you upload your player.

### Memory
An important concept during a game is memory. In this implementation, you can decide what information you want to save between turns, the only condition is it needs to be converted to a `String`. This is shown in the type of the play function:

  type PlayFunc
    =  PlayerId -- ^ this player's Id so they can identify themselves in the bids and tricks
    -> [Card]   -- ^ the player's current hand
    -> [(Card, PlayerId)]   -- ^ cards played in the current trick, i.e., previous players cards for 
                            -- this trick          	 
    -> Maybe ([(Card, PlayerId)], String) -- ^ (all the cards played in the previous trick, "memory")
    -> (Card, String) -- ^ should return: player's chosen card and new memory

The memory is the last parameter to the function, of type: `Maybe` (*cards from the previous trick, previous memory*). 
In the first trick, this will be `Nothing`. Good use of the memory is one of the key features we will evaluate.

### The Tournament
We will run a tournament online based on the code provided. Except the interface, this will be the same game. 

When you submit your player, we will run the test suite on it (stack test). The tests on the server will be slightly longer, your player will compete in:
1. A 1v1 against a basic player.
2. A 1v1 against the minmax player.
3. A 1v1 against the advanced player.
4. A free-for-all, four player match against all the players above.

After the tests have run successfully, your player will join the tournament by immediately playing ten games against selected opponents. After that, it will be selected at random to play against newcomers.

The server for the course is at <https://fit2102.monash> with the following pages:
- [The uploader] (https://fit2102.monash/uploader/): after logging in, this page will allow you to upload your code and compete in the tournament.
- [The ladder] (https://fit2102.monash/ladder.php): this page will display the scores of the last tournament run.

Once you upload your player, you will see two links on the page:
- `home.php`: shows your current ranking, last upload, and previous games played.
- `status.php`: shows the status of your current upload.

Furthermore, you can inspect your games by clicking on their number.

### Game AI
The goal of this assignment is not for you to develop an AI which can compete with openAI or AlphaGo.  The emphasis should be on code quality and applying functional concepts.  Below, you can find a list of standard AI algorithms, ranked roughly by implementation difficulty.  It is possible to receive an HD with a well implemented heuristic player that meets the other HD criteria.  However, students who research and successfully implement the latter strategies will be rewarded.

- **Naïve AI**: tries to play its best card given the current state of the game, you can start by implementing one to make sure you respect the game's rules.
- **Heuristic player**: will save additional information about the game being played to enhance its decision at each turn.
- **Probabilistic player**: will make use of probabilities to determine which cards have the highest chance of winning the game (not the trick).
- **MinMax**: tries to minimise the maximum loss of a player by building a tree of possible moves and playing against itself.  This method was developed for two-player, zero-sum game with perfect information.  In this context, it will be useful in the two-player version but will require modification in the four-player context.
- **Monte Carlo Tree Search**: is the fusion between search algorithms such as minmax and using probabilities to determine the branching in the search tree.  Will also make use of a *simulation* phase to explore deeper.

# Plagiarism
We will be checking your code against the rest of the class, and the internet, using a plagiarism checker. Monash applies strict penalties to students who are found to have committed plagiarism.

Any plagiarism will lead to a 0 mark for the assignment and will be investigated by the staff.  There is a zero-tolerance policy in place at Monash.  So be careful and report any collaboration with other students or other sources.

