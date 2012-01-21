module Deck where
    import Cards
    import Ai
    import Data.List
    import System.Random

    type Deck = [Card]

    fullDeck :: [Card]
    fullDeck = map (\x -> Card x) $ combine (enumFrom Two) (enumFrom Spades)

    combine :: [a] -> [b] -> [(a,b)]
    combine [] _ = []
    combine (x:xs) ys = map (\y -> (x,y)) ys ++ combine xs ys

    shuffleDeck :: Deck -> IO Deck
    shuffleDeck d = shuffleDeck' d 100

    shuffleDeck' :: Deck -> Int -> IO Deck
    shuffleDeck' d n = do
                        pos1 <- genPos
                        pos2 <- genPos
                        xs <- return $ replaceAt (d !! pos1) pos2 d
                        ys <- return $ replaceAt (d !! pos2) pos1 xs
                        if (n == 0) then return ys else shuffleDeck' ys (n-1)

    replaceAt :: a -> Int -> [a] -> [a]
    replaceAt el idx xs = (reverse $ el : drop 1 (reverse firstList)) ++ lastList
                        where (firstList, lastList) = splitAt (idx + 1) xs

    genPos :: IO Int
    genPos = getStdRandom (randomR (0,51))

    dealNCards :: Int -> Deck -> ([Card], Deck)
    dealNCards n deck = (sort $ take n deck, drop n deck)

    type Hand = [Card]
    data Combo = HighCards [Card] | OnePair [Card] | TwoPair [Card] [Card] | ThreeOfAKind [Card] | Straight [Card] | Flush [Card] | FullHouse [Card] [Card] | FourOfAKind [Card] | StraightFlush [Card] deriving (Ord, Eq, Show)

    checkHighCards :: Hand -> Maybe Combo
    checkHighCards hand = case maximum hand of
                            Card(Two,_) -> Nothing
                            card -> Just $ HighCards [card]

    checkOnePair :: Hand -> Maybe Combo
    checkOnePair ((Card (fv,fc)):(Card (sv,sc)):rest) =
      if fv == sv
        then Just $ OnePair [Card(fv,fc),Card(sv,sc)]
        else checkOnePair (Card(sv,sc):rest)
    checkOnePair _ = Nothing

    checkThreeOfAKind :: Hand -> Maybe Combo
    checkThreeOfAKind [] = Nothing
    checkThreeOfAKind ((Card (val,col)):rest) = case (filter (\(Card(ov,oc)) -> ov == val) rest) of
                                           (sc:tc:_) -> Just $ ThreeOfAKind [Card(val,col),sc,tc]
                                           otherwise -> checkThreeOfAKind rest

    checkTwoPair :: Hand -> Maybe Combo
    checkTwoPair ((Card (fv,fc)):(Card (sv,sc)):rest) =
      if (fv == sv)
        then 
          case checkOnePair rest of
            Just (OnePair blah) -> Just $ TwoPair blah [Card (fv,fc),Card (sv,sc)]
            Nothing -> Nothing
        else
          checkTwoPair ((Card (sv,sc)):rest)
    checkTwoPair _ = Nothing

    checkStraight :: Hand -> Maybe Combo
    checkStraight hand = case checkStraight' hand of
                                            True -> Just $ Straight hand
                                            otherwise -> Nothing

    checkStraight' :: Hand -> Bool
    checkStraight' (x@(Card(xc,_)):y@(Card(yc,_)):xs) = if yc == succ xc then checkStraight' (y:xs) else False
    checkStraight' _ = True

    checkFlush :: Hand -> Maybe Combo
    checkFlush ((Card (x,y)):rest) = if all (\(Card(poep,chinees)) -> y == chinees) rest then Just $ Flush ((Card (x,y)):rest) else Nothing
    checkFlush [] = Nothing -- Of wel? Wie zal het zeggen.

    checkFullHouse :: Hand -> Maybe Combo
    checkFullHouse hand = case checkThreeOfAKind hand of
                            Just (ThreeOfAKind (c:rest)) -> case checkOnePair (filter (/= c) rest) of
                                    Just (OnePair cards) -> Just $ FullHouse cards (c:rest) 
                                    otherwise -> Nothing
                            otherwise -> Nothing

    checkFourOfAKind :: Hand -> Maybe Combo
    checkFourOfAKind [c,v,r,x,y] = if c == v && v == r && r == x
                                   then Just $ FourOfAKind [c,v,r,x]
                                   else
                                        if v == r && r == x && x == y
                                        then Just $ FourOfAKind [v,r,x,y]
                                        else Nothing

    checkStraightFlush :: Hand -> Maybe Combo
    checkStraightFlush hand = case checkStraight hand of
                                Just (Straight strcards) -> case checkFlush hand of
                                    Just (Flush fcards) -> Just $ StraightFlush fcards
                                    Nothing -> Nothing
                                Nothing -> Nothing


    comboFunctions = [checkStraightFlush, checkFourOfAKind, checkFullHouse, checkFlush, checkStraight, checkTwoPair, checkThreeOfAKind, checkOnePair, checkHighCards]
    bestCombo :: Hand -> Maybe Combo
    bestCombo hand = case take 1 $ filter (/= Nothing) $ (map (\p -> p hand) comboFunctions) of
                        [] -> Nothing
                        [x]  -> x

    data Result = Win | Lose | Draw deriving (Show)

    whoWin :: Hand -> Hand -> Result
    whoWin yh hh = case (bestCombo yh) `compare` (bestCombo hh) of
                    LT -> Lose
                    EQ -> Draw 
                    GT -> Win

