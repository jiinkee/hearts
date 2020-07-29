-- 29274389 Kee Pei Jiin FIT2102 Assignment 2

{-| Report on the strategy used:
    The strategy implemented is general, hence suitable for both 2-players and 4-players mode.
    The strategy used is conservative, it is not designed to let the player shoot the moon.
    Instead, the main strategy used is to "avoid Queen of Spade as best as the player could".
    This is because Queen of Spade is the card with the highest point, and collecting it in any round of the game will greatly increase the chance of losing the game.
    However, collecting other point cards i.e. Heart cards is inevitable in the process. 

    The collection of point cards is minimised by using the strategy "avoid leading trick as much as possible". This is because the player who collects cards at the end
    of each trick is always a player who leads a trick or follows suit. Leading a trick increases the chance for other opponents to dump cards at the player and thus 
    increases the player's risk of collecting point cards.

    Besides, another strategy that has been implemented is "use cards from shorter suit". A shorter suit is a suit that has lesser remaining card in hand. Using cards
    from shorter suit helps the player to empty suit as fast as possible. Empty suit is a suit where the player does not have any cards of it in hand. Having empty suits
    is good because when the leaad suit of a trick is the player's empty suit, the player can dump cards. The more empty suits the player has, the more chance he/she 
    has to dump dangerous/point cards.

    As Hearts is a complicated game, the strategy is divided into 3 parts:
        1. picking a card when the player leads a round
        2. picking a card when the player is not leading and has to follow suit
        3. picking a card when the player is not leading and cannot follow suit (dumping cards)

    The AI algorithm/ apporach that is being used for the strategy is a combination of Heuristic and simple MinMax.

    For Heuristic algorithm, card is picked based on a few information that are stored in the memory during runtime. These information are:
        - a value indicates if the Heart has been broken (broken Heart value)
        - all used cards
        - players who have run out of Heart cards

    For simple minMax/maxMin algorithm, no game simulation is being implemented (that's why it is called simple). Rather, the main focus of these algorithms is to 
    calculate the lose points for each possible card that the player can play at the current game state. Then, a card will be picked based on the lose point obtained.

    More details of the strategy and algorithm used are documented at the beginning of each section below.
-}

{-| Guide to read the code/ how the code is structured in this file:
    1. the main function
    2. shared functions, these are the functions being used in more than 1 section
    3. functions that implement basic rules
    4. functions for simple minMax algorithm
    5. functions for picking a leading card
    6. functions for picking a following suit card
    7. functions for dumping cards
    8. functions that handle memory (both storing and retrieving values)
-}


module Player (
    playCard,
    makeBid
)
where
import Cards
import Hearts.Types
import Data.Maybe
import Data.List
import Control.Applicative

--------------------------------------------------------------- MAIN FUNCTION ----------------------------------------------------------------------

{- | Main driver function of this program
    The 4 pattern matching below resembles the 4 different situations that player will face in the game, the functions called represent the actions the player
    will take based on the situation. 
    Below are the 4 situations and the respective actions taken:
    1. First trick and none of the opponents have played any card, the player might need to lead the trick if Two of Club is in hand
    2. First trick and someone has played a card, the player needs to obey both bleeding and reneging rules
    3. Other trick and none of the opponenets have played any card, the player won the previous trick and need to lead the current trick
    4. Other trick and someone has played a card, the player needs to obey both breaking and reneging rules

    In all situations, the program will always filter out cards that player cannot play in that current trick before using any strategy to pick a card.
-} 
playCard :: PlayFunc
playCard _ hand [] Nothing = (head $ leading hand, "N\n\n\n")
playCard _ hand trick Nothing = (pickFollowCard (renegeValue hand trick) (reneging (bleeding hand) trick) trick Nothing, "N\n\n\n")
playCard _ hand [] memory = (pickLeadCard (breaking hand (brokenHeart memory)) memory, updateMemoryString memory)
playCard _ hand trick memory = (pickFollowCard (renegeValue hand trick) (reneging hand trick) trick memory, updateMemoryString memory)


-------------------------------------------------------------- SHARED FUNCTIONS --------------------------------------------------------------------
-- | Extract suit from a Card
suit :: Card -> Suit
suit (Card s _) = s

-- | Extract rank from a card
rank :: Card -> Rank
rank (Card _ r) = r

-- | Identify the lead suit of a trick, i.e. the suit of the first card being played in a trick
leadSuit :: [(Card, PlayerId)] -> Suit
leadSuit = suit . fst . last

-- | Retrieve a list of all players in the game
allPlayers :: Maybe ([(Card, PlayerId)], String) -> [PlayerId]
allPlayers = map snd . getLastTrick

-- | Create a list of all poker cards
allCards :: [Card]
allCards = Card <$> [Spade .. Heart] <*> [Two .. Ace]

-- | Retrieve all 13 cards of a given suit
-- Example: Given the suit Spade, return list would be [S2,S3,S4,S5,S6,S7,S8,S9,S10,SJ,SQ,SK,SA], where S2 is a short form of Card Spade Two etc.
allCardBySuit :: Suit -> [Card]
allCardBySuit thesuit = filter (\card -> suit card == thesuit) allCards

-- | Determine if a certain card is within a give card list. Return [] if not found.
findCard :: Card -> [Card] -> [Card]
findCard thecard = filter (== thecard)

-- | Return the cards in a card list which have the specified suit
-- Example: Card list = [SA,S2,HA]; Suit = Spade; Return result = [SA,S2]
findSuit :: [Card] -> Suit -> [Card]
findSuit cards thesuit = filter (\card -> suit card == thesuit) cards

{- | Split a card list into a list of lists, where each inner list contains cards from the same suit
    Example: Card list = [SA,S2,HA]; 
            Suit = [Spade, Heart, Diamond]; 
            Return result = [[SA,S2], [HA], []]
-}
splitBySuit :: [Card] -> [Suit] -> [[Card]]
splitBySuit cardList suits = findSuit cardList <$> suits

-- | Return a list of opponents' cards of the specified suit
-- Opponent cards are those unused cards which are not in the player's hand.
opponentCardofSomeSuit :: [Suit] -> [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card] 
opponentCardofSomeSuit suits hand memory = ((>>=) suits allCardBySuit \\ (>>=) suits (usedCardsBySuit memory)) \\ hand

{-| Given a list of cards splitted by suits, identify which suit(s) is/are the shortest, and return the cards that belong to this suit
    Can be used to determine which cards in hand belong to the player's shortest suit
    Example 1: Card list = [[SA,S2], [HA]
            Return result = [[HA]]
    Example 2: Card list = [[SA,S2], [HA], [DA]]
            Return result = [[HA], [DA]]
-}
lessCardSuit :: [[Card]] -> [[Card]]
lessCardSuit cardsBySuit = filter (\x -> length x == shortestSuitLen) cardsBySuit where
    shortestSuitLen :: Int
    shortestSuitLen = minimum $ map length cardsBySuit

-- | Determine the smallest rank card from a card list and return it as a list
-- Precondition: Cards inside the given list must be of the same suit
smallestRank :: [Card] -> [Card]
smallestRank cards = [minimum cards]

-- | Determine the largest rank card from a card list and return it as a list
-- Precondition: Cards inside the given list must be of the same suit
largestRank :: [Card] -> [Card]
largestRank cards = [maximum cards]



----------------------------------------------------------- FOLLOW RULES ---------------------------------------------------------------
{- Rules of Hearts:
    The purpose of the functions in this section is to filter out cards that player cannot play at every trick, and thus ensure the player
    to only use legal card at every trick. These legal cards will then be passed as input to strategy functions to determine which card
    is a good card to be played.
-}

{-| Leading rule
    Player starts the game if Two of Clubs is in hand.
-}
leading :: [Card] -> [Card]
leading hand = leadgame $ findCard (Card Club Two) hand where
    leadgame :: [Card] -> [Card]
    leadgame [] = bleeding hand
    leadgame c2 = c2

{-| Bleeding rule:
    No point cards (Heart cards and Queen of Spade) can be played in the first trick unless the player does not have any other cards.
    This function filters out all the point cards in the player's hand. If the player does not have any non-point cards, then it will retain
    all cards in player's hand and the player is forced to break the bleeding rule in this situation.
-}
bleeding :: [Card] -> [Card]
bleeding hand = (bleed . filterPointCards) hand where
    filterPointCards :: [Card] -> [Card]
    filterPointCards = filter (\x -> suit x /= Heart) . filter (/= Card Spade Queen)

    bleed :: [Card] -> [Card]
    bleed [] = hand -- player does not have any non-point cards, forced to break bleeding rule
    bleed noPointCards = noPointCards


{-| Breaking rule:
    Player cannot lead with Heart unless Heart has been broken or the player does not have any non-Heart cards. Heart is broken when any Heart card is played.
    Whenever the player leads a trick, the broken heart value is checked.
    If Heart has been broken, broken Heart value = "Y", else, broken Heart value = "N"

    Under different circumstances, this function will filter the player's hand differently:
        1. Heart has been broken. The player's hand is not filtered as it is legal to lead with Heart card.
           Heuristic algorithm is implemented to determine if it is safe to lead with Heart (refer to the section PICK LEADING CARD).

        2. Player does not have any non-Heart cards and is forced to break Heart. In this case, the player will lead with smallest rank Heart, because using a 
            smaller card reduces the player's chance of collecting cards in that trick.
        
        3. For other circumstances, player is not allowed to lead using Heart. So, this function filters out all Heart cards in the player's hand.
            Heuristic algorithm has been implemented to choose a good leading card (refer to the section PICK LEADING CARD).
-}
breaking :: [Card] -> String -> [Card]
breaking hand "Y" = hand
breaking hand _ = (checkIfAllHearts . filterHearts) hand where
    filterHearts :: [Card] -> [Card]
    filterHearts = filter (\card -> suit card /= Heart)

    checkIfAllHearts :: [Card] -> [Card]
    checkIfAllHearts [] = smallestRank hand -- player only have Hearts in hand, forced to break Hearts
    checkIfAllHearts noheart = noheart -- filtered hand without any Heart cards

{-| Reneging rule:
    When the player is not leading a trick, he/she must play a card same suit as the leading suit if there is such card in the player's hand.
    Leading suit is the suit of the first card being played in a trick.

    This function filters the player's hand such that only cards that are following suit are remained. If there is no any following suit cards,
    the player will be forced to break the reneging rule and thus the function will retain all cards in the player's hand as he/she can play with 
    any cards that he/she has now (dump cards).

    Strategy has been used to pick a following suit card/ a card to be dumped (refer to PICK FOLLOWING SUIT CARD & PICK CARD WHEN DUMPING)
-}
reneging :: [Card] -> [(Card, PlayerId)] -> [Card]
reneging hand trick = renege $ findSuit hand (leadSuit trick) where
    renege :: [Card] -> [Card]
    renege [] = hand -- player does not have cards same suit as leader, forced to dump cards of a different suit
    renege samesuit = samesuit

{-| This function is a supplementary function of Reneging rule. 
    It returns a Boolean value that tells if the player is following suit for a certain trick.
    It returns True if the player is following suit and False if otherwise.
-}
renegeValue :: [Card] -> [(Card, PlayerId)] -> Bool
renegeValue hand trick 
    | findSuit hand (leadSuit trick) == [] = False
    | otherwise = True

---------------------------------------------------------- SIMPLE MINMAX ALGORITHM -------------------------------------------------------------
{-| Overview of simple minMax/maxMin algorithm:
    The concept of minmax is being used in the sense that the player will calculate lose point for all possible cards that he/she can play in that trick 
    and pick a card that will minimize the player's chance to lose during that trick.

    The lose point is determined by comparing each player's card in hand with all opponent's cards.
    The lose point for each card is calculated as below:
        1. When the suit of card in hand is different from the suit of opponent's card, lose point + 1
        2. When the suits of two cards are the same but the rank of card in hand is larger than the rank of opponent's card, lose point + 1

    Rationale behind the calculation:
        1. When the suit of player's card (let's call this suit A) is different from many opponent's cards, it indicates one/more of the following:
                - most cards in suit A have been played
                - most cards of suit A are in the player's hand
                - there are very few opponents that have cards of suit A/ there are many opponents that have run out of that suit
            Generally, a card from suit A is not a good card because when it is used to lead a trick, it will increase the chance for other opponents 
            to dump cards at the player, and thus increase the player's risk of collecting point cards.
            Therefore, it is considered that the player has lost if he/she plays a card that has a suit different from many opponent's cards.

        2. Player who follows suit and uses the largest rank card will collect all the cards played in that trick. 
            Once a player collects cards during a trick, there will be a chance for him/her to collect point card(s).
            Hence, it is considered that the player has lost if he/she plays a card that has larger rank than opponent's card.
            
    The minmax algorithm implemented does not have any game simulation, the lose point is calculated for the current trick only. However, its 
    performance is enhanced by the combination of heuristic algorithm (for more heuristic algorithm details, please refer to PICK FOLLOWING SUIT CARD &
    PICK CARD WHEN DUMPING)
-}

{-| This function calls other functions to calculate the lose point for all the player's cards and return the card with the minimum lose point.
    It is used at the scenarios where the player has chance to collect point cards, i.e. when the player leads a trick/ follows suit.
    Using a card with minimum lose points can minimise the player's chance of losing the trick compared to using other cards.
-}
minMax :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
minMax hand memory = [hand !! (bestCardPos $ losePointList hand opponentCards)] where
    opponentCards :: [Card]
    opponentCards = opponentCardofSomeSuit [Spade, Heart, Diamond, Club] hand memory

-- | Determines the position of the first card in hand which has the lowest lose point
bestCardPos :: [Int] -> Int
bestCardPos losePoints = fromJust $ findIndex (== minLosePoint) losePoints where
    minLosePoint :: Int
    minLosePoint = minimum losePoints

{-| This function calls other functions to calculate the lose point for all the player's cards and return the card with the maximum lose point.
    It is used at the scenario where the player does not need to collect any cards, i.e. when the player is dumping card.
    Using a card with maximum lose point at such scenario helps the player to remove high risk/bad card and thus maximising the player's chance of
    NOT losing in the following tricks.
-}
maxMin :: [Card] ->  Maybe ([(Card, PlayerId)], String) -> [Card]
maxMin hand memory = [hand !! (bestCardtoDumpPos $ losePointList hand opponentCards)] where
    opponentCards :: [Card]
    opponentCards = opponentCardofSomeSuit [Spade, Heart, Diamond, Club] hand memory

-- | Determines the position of the first card in hand which has the highest lose point
bestCardtoDumpPos :: [Int] -> Int
bestCardtoDumpPos losePoints = fromJust $ findIndex (== maxLosePoint) losePoints where
    maxLosePoint :: Int
    maxLosePoint = maximum losePoints

{- | Compares each card in player's hand with all remaining opponents' cards and determines the lose points for each card.
    The lose points of each card is returned as a list, where the position of the lose point corresponds to the position of player's card in hand.
    Example: cards in hand = [S2, S5]
            opponents' cards = [S3, H2]
            return result = [1, 2]
-}
losePointList :: [Card] -> [Card] -> [Int]
losePointList [] _ = []
losePointList (x:xs) opponentCards = (calcLosePoint x) ++ (losePointList xs opponentCards) where
    -- before applying the function sumLosePoint, the lose points of a card is a list of 1 and 0, where the length of the list is the total number of remaining
    -- opponent's cards, and each point in the list is the lose point the player's card gets when it is being compared to the opponent's card at corresponding position.
    -- Example: myCard = S5
    --          opponent's cards = [S4, S6, HA]
    --          resulting list before folding = [1, 0, 1]
    calcLosePoint :: Card -> [Int]
    calcLosePoint myCard = sumLosePoint $ (countLosePoint myCard) <$> opponentCards 

-- | Implements the rules of lose point calculation 
countLosePoint :: Card -> Card -> Int
countLosePoint myCard oppCard
    | suit myCard /= suit oppCard = 1
    | rank myCard > rank oppCard = 1
    | otherwise = 0

-- | Adds up the lose points of a card after comparing it with all remaining opponenet's cards
sumLosePoint :: [Int] -> [Int]
sumLosePoint losePoints = [foldr (+) 0 losePoints]


------------------------------------------------------------- PICK LEADING CARD ------------------------------------------------------------------
{-| Strategy to pick a card when player is leading a trick:
    The main strategy used is to avoid leading tricks continuosly. This is because the player who collects cards at the end of each trick is always 
    a player who leads a trick or follows suit. Leading a trick increases the chance for other opponents to dump cards at the player and thus 
    increases the player's risk of collecting point cards.

    Therefore, generally, the smallest card in hand is used.

    The main algorithm of this strategy involves the use of a complex heuristic decision tree, in which each decision factor leads to a branching in the tree.
    Each decision factor is implemented as one or more functions below.

    Below is the text representation of the decision tree:
    1. (Root) Check if hand has SQ/SK/SA
        2. (Has SQ/SK/SA) Check if hand has Hearts
            4.  (has Hearts) Check if number of opponent's Heart cards that is smaller than the player's smallest Heart < number of players who have not run out of Heart
                8. (<) Lead with smallest rank Heart *
                9. (>=) Check if hand has Diamonds/Clubs
                    10. (has Diamond/Club) Check if hand has BOTH Diamonds & Clubs
                        12. (has BOTH) Check if the number of Diamond cards == number of Club cards
                            16. (==) Use minMax algorithm to choose a better card between the smallest Diamond and the smallest Club *
                            17. (/=) Lead with smallest rank card from the shorter suit *
                        13. (does not have BOTH) Lead with the smallest rank card of the suit that player has *
                    11. (does not have Diamond/Club) Check if hand has any Spades other than SQ, SK & SA
                        14. (has other Spades) Lead with smallest rank Spade *
                        15. (does not have other Spades) Check if hand has SQ
                            18. (has SQ) Check if S2 .. SJ have been used && SK/SA has not been used && SK/SA not in hand
                                20. (True) Use SQ *
                                21. (False) Goes to node 19
                            19. (does not have SQ) Check if hand has SK
                                22. (has SK) Use SK *
                                23. (does not have SK) Use SA *
            5.  (does not have Hearts) Goes to node 9
        3. (Does not have SQ/SK/SA) Check if hand has any other Spades
            6. (has other Spades) Lead with smallest rank Spade *
            7. (does not have other Spades) Goes to node 2
    
    Legend:
    * -- leaves
    Number -- the node number
    (condition from the parent decision factor) -- another decision factor that will be used if the condition in () is satisfied
    Nodes that are at the same indentation level branch from the same parent node

    Rationale behind each decision factor is explained below, at the related function.
-}

{-| Main function for picking a card to lead a trick
    Before calling any function that represents the decision tree, the length of hand is checked.
    If there is only one card left, no decision factor is needed to pick a card, so the function will straightaway return this card.
    Else, the function will call the function that represents the root of decision tree.
-}
pickLeadCard :: [Card] -> Maybe ([(Card, PlayerId)], String) -> Card
pickLeadCard hand memory 
    | length hand == 1 = head hand
    | otherwise = head $ gotQKASpade hand memory
 
{-| Node 1. Root of decision tree
    Queen of Spade is the point card that player wants to avoid the most.
    If the player has SQ, the best time to play it would be when the player is dumping card/ following suit and opponent plays SK/SA.
    SK and SA are dangerous cards as well, because it gives a chance to the opponent to dump SQ to the player.
    Thus, if the player has SK/SA, the best time to play them would be when the player is dumping card.
    This implies that the player needs to preserve the other Spade cards as many tricks as possible so that he/she can survive more Spade trick 
    before he/she has the chance to dump SQ/SK/SA.
    Hence, the player needs to avoid leading with Spades when he/she has SQ/SK/SA.

    The other way round, when the player does not have SQ/SK/SA, the player should try to lead with Spades so that the opponent is forced to use his/her
    Spade card. This can prevent the opponent from surviving many Spade tricks and has the chance to dump SQ/SK/SA.
-}
gotQKASpade :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
gotQKASpade hand memory
    | findLargeSpade hand = tryLeadWithHeart hand memory
    | otherwise = tryLeadWithSpade hand memory
    where
        findLargeSpade :: [Card] -> Bool
        findLargeSpade = any (\card -> card `elem` largeSpade)

        largeSpade :: [Card]
        largeSpade = Card <$> [Spade] <*> [King, Queen, Ace]

{-| Node 3
    Lead using the smallest rank Spade card if possible.
    If player does not have any Spades, try to lead trick using Heart.
-}
tryLeadWithSpade :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
tryLeadWithSpade hand memory = anyOtherSpade $ findSuit hand Spade where
    anyOtherSpade :: [Card] -> [Card]
    anyOtherSpade [] = tryLeadWithHeart hand memory -- Node 7: tried to lead with Spades but does not have any Spade card
    anyOtherSpade otherSpades = smallestRank otherSpades -- Node 6 *

{-| Node 2
    Player's hand has been filtered in playFunc before it is being passed into the functions in this section, so only legal cards are passed into 
    the functions here, i.e. all Heart cards in the player's hand have been filtered out if Heart has not been broken unless the player does not have
    any non-Heart cards. If hand has any Heart cards at this stage, it indicates that it is legal to lead using Heart cards.

    As Heart cards are point cards, they cannot be used carelessly. However, when it is used wisely, it can cause the opponents to take more damage during 
    that trick. Therefore, in the case where the player has Hearts in hand, a conservative aggressive strategy is implemented to decide whether it is safe
    to lead using Heart. If the player does not have any Hearts in hand, the player will try to lead trick with Diamond/Club.
-}
tryLeadWithHeart :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
tryLeadWithHeart hand memory 
    | (findSuit hand Heart) == [] = tryLeadWithDC hand memory -- Node 5
    | otherwise = safeToLeadHeart hand memory

{-| Node 4
    Conservative aggresive algorithm: Only attack with Heart cards under complete safe conditions
    It is only safe to lead with Hearts under one condition:
        the number of opponent's Heart cards that are smaller than the player's smallest Heart (n) < the number of players who have not run out of Hearts (p)
    This condition ensures that there is at least one opponent will have Heart cards larger than the player's smallest Heart. Hence, if the player leads with 
    his/her smallest Heart card, at least one opponent will be forced to use a Heart card that is greater than the player's card (due to reneging rule), and thus
    it is confirmed that the player will not collect any point cards during that trick.
    
    e.g. n = 1, p = 2
    The smaller opponent Heart card can only belongs to one of the opponents, hence, the other surely has a Heart card that is larger than the player's smallest Heart

    If the condition is not fulfilled, the player will not take any risk to lead with Hearts, instead, he/she will try to lead using Diamond/Club cards.
-}
safeToLeadHeart :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
safeToLeadHeart hand memory 
    | getN hand memory < getP memory = smallestRank $ findSuit hand Heart -- Node 8 *
    | otherwise = tryLeadWithDC hand memory

{-| Supplementary function for safeToLeadHeart
    Determine n, the number of opponents' Heart cards that are smaller than the player's smallest Heart.
    Opponent's Heart cards are those unused Heart cards that are not in the player's hand.
-}
getN :: [Card] -> Maybe ([(Card, PlayerId)], String) -> Int
getN hand memory = anyOppHeart $ opponentCardofSomeSuit [Heart] hand memory where
    anyOppHeart :: [Card] -> Int
    anyOppHeart [] = 0 -- all other opponents do not have Heart
    anyOppHeart opponentHearts = length $ compareWithSmallestH opponentHearts (findSuit hand Heart) 

-- | Returns only the opponent's Heart cards that are smaller than the player's smallest Heart card
compareWithSmallestH :: [Card] -> [Card] -> [Card]
compareWithSmallestH opponentHearts myHearts = filter (\card -> card < minimum myHearts) opponentHearts

{-| Supplementary function for safeToLeadHeart
    Determine p, the number of players that have not run out of Hearts
-}
getP :: Maybe ([(Card, PlayerId)], String) -> Int
getP memory = length $ allPlayers memory \\ whoRunOutHeart memory -- whoRunOutHeart is a function declared in the MEMORY HANDLING section

{-| Node 9
    To reach until this node, there are only 2 possible hand cards combinations:
        1. has SQ/SK/SA + does not have Heart cards + may have other Spade cards
        2. only has Diamond and Club cards in hand
    If the player does not have Diamond/Club cards, this implies that the player's hand cards combination is 1. Hence, the player can only lead with Spade card.
    If otherwise, this implies that the player's hand cards combination is 2. Hence, the player can only lead with Diamond/Club card.
-}
tryLeadWithDC :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
tryLeadWithDC hand memory = anyDC (splitBySuit hand [Diamond, Club]) where
    -- Node 10
    anyDC :: [[Card]] -> [Card]
    anyDC [[],[]] = leadWithSpade hand memory -- card combination 1
    anyDC diamondClub = useDorC (head diamondClub) (last diamondClub) memory -- card combination 2

{-| Node 12
    To reach until this node, it indicates that the player's hand only has Diamond or Club cards.
    This function decides whether to lead using a Diamond or a Club card.
    If the player's hand only has cards of either Diamond or Club, then the player will lead with the smallest rank card of the suit he/she has.
    If the player has both Diamond and Club cards, then the player will play the smallest card from the shorter suit.
    
    A shorter suit is a suit that has lesser cards. The rationale behind playing card from the shorter suit is because it helps the player to empty suit faster,
    which eventually creates more chance for the player to dump cards.

    When the number of Diamond cards in hand == the number of Club cards in hand, no other decision factor can help to pick a card, hence the MinMax algorithm is used.
-}
useDorC :: [Card] -> [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
useDorC [] club _ = smallestRank club -- Node 13: only has Club cards
useDorC diamond [] _ = smallestRank diamond -- Node 13: only has Diamond cards
useDorC diamond club memory 
    | length diamond == length club = minMax [minimum diamond, minimum club] memory -- Node 16: two suits are of same length, use MinMax algorithm
    | otherwise = head $ smallestRank <$> lessCardSuit [diamond, club] -- Node 17 *

{-| Node 11
    To reach until this node, it indicates that the player's hand only has Spade cards.
    This function decides which Spade card is the best card to be played.
    
    If possible, the player needs to avoid leading a trick using SQ/SK/SA because these are high rank card, so if the player plays any of SQ/SK/SA,
    he/she will have high chance of collecting cards during that trick, and hence increases the player's chance of getting point cards. If lead with SQ/SK/SA,
    the worst case scenario would be collecting SQ during that trick, and so the player will get 13 penalty points.

    Therefore, if the player has Spades that are not SQ/SK/SA, the smallest rank of such Spade cards will be used.
    If otherwise, the player has no other choice but to lead using SQ/SK/SA.
-}
leadWithSpade :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
leadWithSpade hand memory = (gotOtherSpade . findOtherSpade) hand where
    largeSpade :: [Card]
    largeSpade = Card <$> [Spade] <*> [King, Queen, Ace]

    findOtherSpade :: [Card] -> [Card]
    findOtherSpade = filter (\card -> not $ card `elem` largeSpade)

    gotOtherSpade :: [Card] -> [Card]
    gotOtherSpade [] = gotQSpade (findCard (Card Spade Queen) hand) hand memory -- does not have other Spade cards, only has SQ/SK/SA
    gotOtherSpade otherSpades = smallestRank otherSpades -- Node 14 *

{-| Node 15
    To reach till this node, it indicates that the player's hand only has SQ/SK/SA.
    Among these 3 cards, the safest card to be played is SQ (because its rank is the smallest). However, since SQ is a point card, it should only be used
    under strict conditions:
        1. All the Spade cards smaller than SQ have been used 
        2. At least one of SK/SA has not been used
        3. At least one of SK/SA is not in the player's hand
    The conditions above are considered as safe for the player to lead using SQ because it ensures that at least one opponent has SK/SA, and due to reneging rule,
    the opponent holding SK/SA has to play SK/SA and hence this opponent will collect cards during that trick.

    Since the player is conservative, as long as the conditions are not fulfilled, he/she will not take the risk to lead using SQ. Instead, he/she will lead using 
    the other cards which are SK/SA. This strategy does not completely eliminate the chance for player to collect SQ, but it has minimised the chance as much as
    possible. If the player really happens to collect SQ, it is inevitable as the player has run out of all possible cards that could be played.

    Note: the function does not consider the case where the player's hand has only SQ, because the case where player's hand has only one card left in it has
          been handled in the main function pickLeadCard. Therefore, for the functions implemeting the decision tree to be used, the player's hand must have
          at least 2 cards in it.
-}
gotQSpade :: [Card] -> [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
gotQSpade [] hand _ = useKorASpade hand
gotQSpade _ hand memory
    | usedAllSmallSpade && unusedKA && kaNotInHand = [Card Spade Queen] -- Node 18 & 20 *: check if it is safe to lead using SQ
    | otherwise = useKorASpade hand -- Node 21 *
    where
        -- | check if all small Spade cards smaller than SQ have been used
        usedAllSmallSpade :: Bool
        usedAllSmallSpade = (length $ filter (< Card Spade Queen) (usedCardsBySuit memory Spade)) == 13

        -- | check if SK/SA/Both have been used before
        unusedKA :: Bool
        unusedKA = (length $ filter (> Card Spade Queen) (usedCardsBySuit memory Spade)) < 2

        -- | check if SK/SA/Both are in opponent's hand
        kaNotInHand :: Bool
        kaNotInHand = (length $ [Card Spade King, Card Spade Ace] \\ hand) > 0

{-| Node 19
    The player will only reach this node if it is not safe to lead using SQ or the player has only SK/SA.
    Between these two cards, SK is relatvely better because its rank is smaller.
    Hence, SA will only be used if and only if the player does not have SK.
-}
useKorASpade :: [Card] -> [Card]
useKorASpade hand 
    | (findCard (Card Spade King) hand) == [] = [Card Spade Ace] -- Node 23 *: only has SA
    | otherwise = [Card Spade King] -- Node 22 *: only has SK or Both

---------------------------------------------------- PICK CARD WHEN PLAYER IS NOT LEADING --------------------------------------------------------

{-| Main function for picking a card when the player is not leading the trick

    Before calling any functions that implement strategy for picking a card, the length of hand is checked.  
    If there is only one card left in the player's hand, the player does not have other cards to play, so the function will straightaway return the card.
    Else, the renege value will be checked to see if the player follows suit during that trick. Renege value is True if the player is following suit and False if
    otherwise.
    This value is important because different strategy will be used depending on the situation.
-}
pickFollowCard :: Bool -> [Card] -> [(Card, PlayerId)] -> Maybe ([(Card, PlayerId)], String) -> Card
pickFollowCard renegeVal hand trick memory 
    | length hand == 1 = head hand
    | renegeVal == True = head $ followSuitCard hand trick memory
    | otherwise = head $ dumpCard hand memory

------------------------------------------------------ PICK FOLLOWING SUIT CARD ------------------------------------------------------------------
{-| Strategy to pick a following suit card:
    
    Due to reneging rule, the heuristic algorithm that can be implemented for strategy to pick a following suit card is very restricted. 
    Therefore, the heuristic algorithm is combined with the minmax algorithm to improve the decision made using the strategy.

    Below is a brief description of the heuristic algorithm implemented:
    When the player is following suit, it indicates that the player has a chance of collecting cards during that trick.
    Hence, to minimise the chance of collecting cards, the player needs to play his/her safe largest card. The safe largest card is the player's
    largest following suit card which is smaller than the largest following suit card in the trick. This ensures that there is at least one opponent has 
    a played a larger card than the player during that trick, and hence this opponent will collect the cards.

    It is possible that the players have more than one cards that are smaller than the largest following suit card in trick, but the player will always play
    the largest card among all these cards because there are more disadvantages of keeping large rank card than using them. Keeping large rank card in hand
    means that there is a chance for the player to play it when he/she is leading a trick, and using a large rank card while leading is not good as it will 
    increase the player's chance of collecting point cards.

    If the player does not have any card that is smaller than the largest following suit card in trick, it cannot be guranteed that the player will not collect cards
    during that trick anymore. Therefore, minmax algorithm is used to pick a card which has the minimum lose point, i.e. a card that will minimise the 
    player's risk of collecting cards during that trick.

    Note: Opponents who have played cards different from the lead suit are not taken into consideration because they will not collect cards during that trick anyways.
          What the player needs to do is to make sure that there is one other opponent who will collect the cards during the trick.
-}

{-| Main function for picking a following suit card

    During the first trick, no point cards are allowed to be played (due to bleeding rule), thus, it is safe for the player to use his/her largest Club
    because there is no damage even if the player collects cards in this trick. 
    Besides, in the case where the player really collects cards in the first trick, it is quite safe for the player to lead the second trick because it is
    very unlikely to have players who run out of suit in the second trick (unless the card distribution is highly uneven). Therefore, there is a very low chance for
    opponents to dump cards at the player in the second trick. Based on the leading card strategy implemented, it is very likely that the player can avoid leading
    the third trick. Hence, it is generally safe to use the largest Club in hand during the first trick.
    
    As for other tricks, the best strategy would be using the player's safe largest card whenever possible as explained above.
    If there is no safe lergest card in the player's hand, minmax algorithm will be used to determine the high rank card that has the minimum lost.
-}
followSuitCard :: [Card] -> [(Card, PlayerId)] -> Maybe ([(Card, PlayerId)], String) -> [Card]
followSuitCard hand _ Nothing = largestRank hand -- the first trick
followSuitCard hand trick memory = possibleCards $ safeFollowSuitCards hand trick where -- other tricks
    possibleCards :: [Card] -> [Card]
    possibleCards [] = minMax hand memory
    possibleCards smallerCards = largestRank smallerCards -- play the safe largest card

-- | Retrieve a list of player's cards that are smaller than the largest following suit card in trick
safeFollowSuitCards :: [Card] -> [(Card, PlayerId)] -> [Card]
safeFollowSuitCards hand trick = filter (\card -> card < largestFSCardInTrick trick) hand

-- | Select the largest following suit card played by opponent in the trick
largestFSCardInTrick :: [(Card, PlayerId)] -> Card
largestFSCardInTrick = maximum . followSuitCardInTrick

-- | Retrieve a list of following suit cards played by opponents in the trick
followSuitCardInTrick :: [(Card, PlayerId)] -> [Card]
followSuitCardInTrick trick = findSuit (cardsInTrick trick) (leadSuit trick)

-- | Retrive a list of cards played by the opponents from the current trick
cardsInTrick :: [(Card, PlayerId)] -> [Card]
cardsInTrick = map fst

--------------------------------------------- PICK CARD WHEN DUMPING (NOT FOLLOWING SUIT) ------------------------------------------------------------

{-| Strategy to pick a card to dump
    
    Dumping card is the part of the game that gives the player the most benefits. This is because when the player dumps card, it is guranteed that he/she will
    not collect any cards during that trick; in fact, the player can even bring damage to the opponents if he/she dumps card wisely, i.e. dumps point card

    Heuristic algorithm is implemented to decide the card to be dumped. Generally, the cards that the player should dump are those cards that can increase the
    player's risk of collecting cards in the future tricks/ point cards.
    The sequence of card to be dumped is as below:
        1. SQ
        2. SA
        3. SK
        4. HA
        5. DA/CA
        6. The last card in a suit in hand 
        7. Highest rank Heart card
        8. Highest rank card from the player's shortest suit
    The rationale behind this dumping seqeuence is explained below at the related function.
-}

{-| Main function for dumping cards
    This function calls another function that starts finding cards to be dumped according to the dumping sequence
-}
dumpCard :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
dumpCard hand memory = dumpDangerous (Card Spade Queen) (findCard (Card Spade Queen) hand) hand memory

{-| Dump dangerous/high risk cards (Dumping sequence 1 - 4)

    SQ is the point card that the player wants to avoid the most. Therefore, if the player's hand has SQ, it needs to be dumped to the other opponents as fast 
    as possible so that the player can be safe from collecting SQ in the future tricks. Hence, SQ is given the highest priority in the dumping sequence.

    Both SK and SA are cards that will increase the the player's risk of collecting SQ. e.g. During a Spade trick, opponent has played SQ, but the player has only
    SK/SA, player must play SK/SA due to reneging rule and hence collect SQ in this process. Therefore, SA/SK need to be dumped as fast as possible as well to
    lower the player's risk of collecting SQ in the future tricks.

    HA is also a dangeroud card because it is the largest card in the Heart suit. Hence, it increases the player's risk of collecting point cards in the future tricks.
    e.g. During a Heart trick, the only Heart card the player has is HA, the player must play HA due to reneging rule. Since HA is the largest Heart, the player has
    no choice but to collect all the point cards.
    However, HA is given lower priority in the dumping sequence compared to SQ, SK and SA because even in the worst case scenario where the player collects 4 point
    cards, the damage of these point cards (4) is still smaller than the damage of one SQ (13).
-}
dumpDangerous :: Card -> [Card] -> [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
dumpDangerous card [] hand memory -- continue searching for dangerous cards in the player's hand according to the dumping sequence
    | card == Card Spade Queen = dumpDangerous (Card Spade Ace) (findCard (Card Spade Ace) hand) hand memory
    | card == Card Spade Ace = dumpDangerous (Card Spade King) (findCard (Card Spade King) hand) hand memory
    | card == Card Spade King = dumpDangerous (Card Heart Ace) (findCard (Card Heart Ace) hand) hand memory
    | otherwise = dumpADC (findCard (Card Diamond Ace) hand) (findCard (Card Club Ace) hand) hand memory
dumpDangerous _ cardFound _ _ = cardFound -- the dangerous card is in the player's hand, hence the player dumps it

{-| Dumping sequence 5

    Ace is the largest card in all suit, indicating the player who follows suit and plays an Ace card will always collect cards during the trick.
    Collecting cards is not good because it increases the player's risk of getting point cards.
    Therefore, Ace cards is given priority in the dumping sequence. Since Spade and Heart are the suits where point cards belong to, SA nad HA are given higher priority
    in the dumping sequence compared to DA and CA.

    The sequence of dumping DA and CA is not very important. However, to minimise the player's chance of losing in the future tricks, in the case where the player 
    has both DA and CA, the player should dump the card from suit where more opponents have run out of. This is because if the player holds onto such cards, it means 
    that there is a chance for the player to lead using this card in the future tricks. And when the player leads with such card, it opens chances for opponents
    to dump cards at the player and hence increases the player's risk of collecting point cards.

    To determine such card, the MaxMin algorithm is used. (as for why MaxMin can help determining this card, please refer to the documentation under SIMPLE MINMAX 
    ALGORITHM section)
-}
dumpADC :: [Card] -> [Card] -> [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
dumpADC [] [] hand memory = dumpOneLastCard hand memory
dumpADC aDiamond [] _ _ = aDiamond
dumpADC [] aClub _ _ = aClub
dumpADC aDiamond aClub _ memory = maxMin (aDiamond ++ aClub) memory -- have both DA and CA, use MaxMin to determine which suit has lesser opponent that run out of it

{-| Dumping sequence 6
    
    When the player's hand does not have all the dangerous cards above, the player should try to dump card to empty suit in hand.
    e.g. If the player has only one Spade card remaining in hand, the player should dump this one last card to empty the Spade suit in hand.
    Having empty suit is generally good for the player because it gives the player more chance to dump cards in the future tricks.
    e.g. The player dumps his/her last Spade card during the current trick. Then, for all the future Spade tricks, the player does not need to obey the reneging rule 
    and he/she can dump dangerous cards/point cards.

    One last card in suit is given higher priority in the dumping sequence compared to Heart cards because emptying suit in hand gives the player more chance to
    dump card, and with more dumping chances, the player can dump more Heart cards afterwards. 

    This function determines the suits in the player's hand that have only one card remaining, and returns the card in this suit for the player to dump.
    If there is no suit in hand that has one last card, the player cannot empty any suit yet, and thus will dump Heart cards.
    If the player's hand has more than one suits that have one last card, the MaxMin algortihm is used to determine which one last card has the highest lose point and
    the player will dump this card. Dumping a card that has higher lose points can reduce the player's risk of losing in the future tricks.
-}
dumpOneLastCard :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
dumpOneLastCard hand memory = (almostEmptySuit . leftOneLastCard) (splitBySuit hand [Spade, Club, Heart, Diamond]) where
    almostEmptySuit :: [[Card]] -> [Card]
    almostEmptySuit oneLastCards
        | length oneLastCards == 1 = head oneLastCards -- only one suit in hand has one last card
        | length oneLastCards == 0 = dumpHeart hand memory -- none of the suit in hand has only one card left, try dump heart cards
        | otherwise = maxMin (foldr (++) [] oneLastCards) memory -- oneLastCards is a list of lists, needs to flatten it before passing into the function maxMin
    
    -- | Filters out empty suit and suits that have more than one card remaining in the player's hand
    -- Returns a list of lists, each inner list is the one last card in a suit
    -- E.g. cards in hand = [[S2,S3], [C4]]
    --      filtered cards = [[C4]]
    leftOneLastCard :: [[Card]] -> [[Card]]
    leftOneLastCard = filter(\x -> length x == 1)

{-| Dumping sequence 7

    When the player's hand does not have any dangerous cards or suits that have only one card remaining, the player will dump Heart cards if he/she has any Heart cards.
    
    Dumping Heart cards is given higher priority in the dumping sequence compared to high rank cards of other suits because dumping Heart cards will add the points of
    opponent who collects cards during that trick as well as reduces the player's risk of collecting cards in the future Heart tricks. On the other hand, dumping a 
    high rank card can only achieve the latter purpose but cannot bring "damage" to the opponent.

    If the player has Heart card, the player should dump his/her largest Heart card. Dumping the largest Heart can further reduce the player's risk of collecting cards
    during a future Heart trick.
    If the player does not have a Heart card, the player will dump his/her largest card of the shortest suit.

    Note: No point cards are allowed in the first trick. Therefore, in the case where it is the first trick, and the player does not have any dangerous cards above,
         the player will just dump his/her largest rank card in the hand. Length of suit is not taken into consideration because generally, the lengths of each 
         suit in the player's hand in the first trick are more or less the same. Hence, the suit length is not very helpful in picking card during the first trick.
-}
dumpHeart :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
dumpHeart hand Nothing = largestRank hand -- during the first trick
dumpHeart hand memory = anyHeart $ findSuit hand Heart where -- other tricks
    anyHeart :: [Card] -> [Card]
    anyHeart [] = dumpShortSLargeC hand memory
    anyHeart heartsInHand = largestRank heartsInHand

{-| Dumping sequence 8

    The player will dump his/her largest card from his/her shortest suit in hand when the player does not have any dangerous cards and Heart cards.
    
    Dumping a high rank card is beneficial to the player because it reduces the player's risk of collecting cards in the future tricks.
    But why dump card from shorter suit? This is because a shorter suit is a potential empty suit, dumping cards from shorter suit can empty that suit faster and hence
    gives the player more chance to dump in the future tricks.

    This function determines the suits in player's hand that have the least card remaining. Note that empty suit is not included in the suit length comparison.
    If there is only one shortest suit in hand, the function will return the largest card of this suit.
    If there are more than one suits in hand that have the same least amount of card, the player should dump the card from suit where more opponents have run out of,
    so that it decreases the opponent's chance to dump card at player, and hence decreases the player's chance of collecting point cards.
    
    MaxMin algorithm is used to determine this card. As to why MaxMin algorithm can be used in this case, please refer to the documentation under the SIMPLE 
    MINMAX ALGORITHM section.
-}
dumpShortSLargeC :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
-- Since the player does not have any Heart cards, there is no point of splitting the player's hand by the Heart suit
dumpShortSLargeC hand memory = (allShortSuits . lessCardSuit . filterEmptySuit) (splitBySuit hand [Spade, Club, Diamond]) where
    filterEmptySuit :: [[Card]] -> [[Card]]
    filterEmptySuit = filter (\x -> length x /= 0) 

    allShortSuits :: [[Card]] -> [Card]
    allShortSuits shortSuitInHand -- shortSuitInHand is a list of lists, where each inner list are cards from the same suit
                                  -- Example of shortSuitInHand = [[S2,S4], [C3,C5]]
        -- For each suit (inner list), find the largest card. Among all these cards, return the one with the highest rank
        -- The maximum function cannot be used to find the highest rank card among cards of different suits because it will compare the card suit first before it compares card rank
        | length shortSuitInHand == 1 = highRankCard $ largestRank =<< shortSuitInHand 
        -- shortSuitInHand is a list of lists, so it is flattened before being passed into the maxMin function
        | otherwise = maxMin (foldr (++) [] shortSuitInHand) memory 


-- | Given a list of cards, determine the highest rank among these cards and filter the card list such that only cards of that rank remain
-- This function works for both cards from the same suit as well as different suits
highRankCard :: [Card] -> [Card]
highRankCard cards = [head $ filter (\x -> rank x == highestRank) cards] where
    highestRank :: Rank
    highestRank = (maximum . map rank) cards


------------------------------------------------------------- MEMORY HANDLING --------------------------------------------------------------------------

{-| Structure of memory string:
    The type of memory is : Maybe ([(Card, PlayerId)], String)
    [(Card, PlayerId)] : the list of cards being used in the last round
    String : memory string

    The information being stored in memory string are as follow:
        1. brokenHeartValue : "Y"/"N"
        2. All used cards (except those were being used during the latest last trick) e.g. "SA H7 D9"
        3. PlayerIds that run out of Heart cards e.g. "1 2 3"

    The delimiter used between two different information is "\n".
    The delimiter used between two values inside one information is " ".

    Example of memory string: "Y\n2 3\nS2 S4 H7 H5"

    For each information, there are functions that update the string and retrieve data from the string. Therefore, the functions below are further divided into
    subsections based on the information that the function is handling, i.e. functions for updating and retrieving data of the same information are grouped under one 
    subsection.

    The cards need to stored as string, but when the used cards information is retrieved, we need it to be in Card type. Hence, there are functions that can convert 
    the card from string to Card type and vice versa.

    During information retrieval, the memory string will be split into list because it is easy to extract data from a list structure.
-}

-- | Main function that update the memory string at each trick
--   This function calls three other functions which update each part of information in the memory string.
--   Between each information, newline is added as delimiter.
updateMemoryString :: Maybe ([(Card, PlayerId)], String) -> String
updateMemoryString memory = brokenHeart memory ++ "\n" ++ updateUsedCards memory ++ "\n" ++ updateRunOutSuit memory


-- | Retrieve the last trick from the memory  
getLastTrick:: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)]
getLastTrick = fst . fromJust


-- | Retrieve memory string from the memory
getMemoryString :: Maybe ([(Card, PlayerId)], String) -> [String]
getMemoryString = lines . snd . fromJust


--------------------------------------------------------------- BROKEN HEART VALUE --------------------------------------------------------------

{-| Retrieve the first information stored in memory string: Broken Heart Value
    If heart is broken, broken heart value = "Y", else, broken heart value = "N"
    This broken heart value does not consider the cards being played in the latest last trick
-}
getBrokenHeart :: Maybe ([(Card, PlayerId)], String) -> String
getBrokenHeart = head . getMemoryString 

{-| Update Broken Heart Value/ Check the broken heart value (for all rounds including the latest last trick)
    Once Heart has been broken, it is broken. Therefore, if the broken heart value is "Y", the new broken heart value will remain as "Y".
    If Heart has not been broken before, then the function will check if any Heart has been played in all the previous tricks (include the latest last trick)
    If Heart has been played, the broken heart value will change from "N" to "Y", else, the value will remain as "N".

    This broken heart value has taken consideration of the cards played in the latest last trick.
-}
brokenHeart :: Maybe ([(Card, PlayerId)], String) -> String
brokenHeart memory 
    | getBrokenHeart memory == "Y" = "Y"
    | usedCardsBySuit memory Heart == [] = "N"
    | otherwise = "Y"

-------------------------------------------------------------- ALL USED CARDS -----------------------------------------------------------

-- | Retrieve the second information stored in memory string: All used cards except the ones being played during the latest last trick
getCardStringOnly :: Maybe ([(Card, PlayerId)], String) -> String
getCardStringOnly =  head . tail . getMemoryString

-- | Main function to retrieve a list of ALL used cards (include the cards being played during the latest last trick) 
getUsedCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
getUsedCards memory = previousTricksCards memory ++ lastTrickCards memory

-- | Retrieve a list of cards played in all previous tricks (except the last trick)
-- This information is retrieved from the "used cards" part being stored in memory string
-- The cards in string are being converted into the type Card
previousTricksCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
previousTricksCards = map stringToCard . words . getCardStringOnly

-- | Retrieve a list of cards played in the latest last trick
-- This information is retrieved from the trick in the memory 
lastTrickCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
lastTrickCards = map fst . getLastTrick

-- | Retrieve all used cards of a certain suit
usedCardsBySuit :: Maybe ([(Card, PlayerId)], String) -> Suit -> [Card]
usedCardsBySuit memory thesuit =  findSuit (getUsedCards memory) thesuit


-- | Main function to update the used cards information in memory string
-- To update the used cards is to add the cards being used in the last trick into the "Used Cards" part of memory string
updateUsedCards :: Maybe ([(Card, PlayerId)], String) -> String
updateUsedCards memory = getCardStringOnly memory ++ " " ++ lastTrickUsedCards memory

-- | Retrieve the cards played in the latest last trick and convert the cards into string, so that it can be stored in the memory string
lastTrickUsedCards :: Maybe ([(Card, PlayerId)], String) -> String
lastTrickUsedCards = intercalate " " . map cardToString . lastTrickCards


-------------------------------------------------------- PLAYERS WHO HAVE RUN OUT OF HEART ------------------------------------------------------

-- | Retrieve the third information stored in the memory string: Player Ids that have run out of Heart cards
getRunOutSuitOnly :: Maybe ([(Card, PlayerId)], String) -> String
getRunOutSuitOnly = last . getMemoryString 

-- | Retrieve a list of Ids of the players who have run out of Heart cards from the memory string
-- split the "Players who run out of Heart" string into a list of Player Ids
-- The delimiter used between two player Ids is " ", hence the function word is used to split the string into list
whoRunOutHeart :: Maybe ([(Card, PlayerId)], String) -> [PlayerId]
whoRunOutHeart = words . getRunOutSuitOnly 

-- | Identify the leading suit of the last trick
lastTrickLeadSuit :: Maybe ([(Card, PlayerId)], String) -> Suit 
lastTrickLeadSuit = leadSuit . getLastTrick 


-- | Update the players who have run out of Heart cards
-- Only update this part of memory string if the leading suit of last trick is Heart. If the lead suit is not Heart, the "Player who run out of Heart" part in the
-- memory string will remain unchanged.
updateRunOutSuit :: Maybe ([(Card, PlayerId)], String) -> String 
updateRunOutSuit memory 
    | lastTrickLeadSuit memory == Heart = playerWhoRunOutHeart memory
    | otherwise = getRunOutSuitOnly memory


-- | Identify the players who run out of Heart cards throughout the whole round, and convert the player Ids into string so that it can be stored in the memory string
playerWhoRunOutHeart :: Maybe ([(Card, PlayerId)], String) -> String
-- the nub function needs to be used because it is possible that there is duplicated player Id in the "Player who run out of Heart" part of memory string and
-- the new player Id being determined as player who have run out of Hearts during the last trick
    -- This can happen because once a player has run out of Heart, he/she will dump card in all the Heart tricks afterwards
    -- Therefore, it is possible to identify the same player as player who has run out of Hearts more than once
playerWhoRunOutHeart memory = intercalate " " (nub $ (whoRunOutHeart memory) ++ (runOutLastTrick memory))
    
-- | Identify the players who run out of Heart card during the latest last trick
-- This can be done by determining players who play a non-Heart card during a Heart trick
runOutLastTrick :: Maybe ([(Card, PlayerId)], String) -> [String]
runOutLastTrick = map snd . filter (\trick -> (suit $ fst trick) /= Heart) . getLastTrick


--------------------------------------------------------- CONVERT SUIT/RANK TO STRING ---------------------------------------------------------

-- | Convert the type of card from Card to String 
cardToString :: Card -> String
cardToString card = (suitToString $ suit card) ++ (rankToString $ rank card)

-- | Convert the type of suit from Suit to String
suitToString :: Suit -> String
suitToString cardSuit = case cardSuit of
    Club -> "C"
    Spade -> "S"
    Diamond -> "D"
    Heart -> "H"

-- | Convert the type of rank from Rank to String
rankToString :: Rank -> String
rankToString cardRank = case cardRank of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "0" -- 10
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

---------------------------------------------------------- CONVERT STRING TO SUIT/RANK ---------------------------------------------------------

-- | Convert the type of card values from String to Card
stringToCard :: String -> Card
stringToCard card = (Card (stringToSuit [head card]) (stringToRank [last card]))

-- | Convert the suit from String to Suit
stringToSuit :: String -> Suit
stringToSuit cardSuit = case cardSuit of
    "C" -> Club
    "S" -> Spade
    "H" -> Heart
    _ -> Diamond -- "D"
    
-- | Convert the rank from String to Rank
stringToRank :: String -> Rank
stringToRank cardRank = case cardRank of
    "2" -> Two
    "3" -> Three
    "4" -> Four
    "5" -> Five
    "6" -> Six
    "7" -> Seven
    "8" -> Eight
    "9" -> Nine
    "0" -> Ten
    "J" -> Jack
    "Q" -> Queen
    "K" -> King
    _ -> Ace -- "A"


----------------------------------------------------------- UNUSED FUNCTIONS -------------------------------------------------------------------
-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined

