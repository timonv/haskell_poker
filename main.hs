module Main where
    import Deck
    import Cards
    import Ai
    import Data.Char

    main :: IO ()
    main = do
            putStrLn "Welcome to Haskell Poker!"
            putStrLn "Type 'No' to quit!"
            result <- getLine
            if (map toLower result) /= "no" then
                do
                 game
                 main
            else
                do
                 putStrLn "Thanks for playing!"

    game :: IO ()
    game = do
              deck <- shuffleDeck fullDeck -- It shuffles the deck one hundred times in prev function
              (phand, deck) <- return $ dealNCards 5 deck
              (chand, deck) <- return $ dealNCards 5 deck
              showHand phand
              cards <- askCardsForExchange
              (phand,deck) <- return $ exchangeCards phand cards deck
              showHand phand
              checkResult phand chand

    -- Since I'm going for the fancy display method, I don't want space seperators but line seperators :P
    showHand :: Hand -> IO()
    showHand hand = do
                       putStrLn ""
                       putStrLn "--- Your Cards ---"
                       mapM_ print hand
                       putStrLn "------------------"
                       putStrLn ""

    askCardsForExchange :: IO [Card]
    askCardsForExchange = do
                            putStrLn "Which cards do you want to discard?"
                            putStrLn "N.b. You need to type '(value,color)', seperated by spaces, as I'm too lazy to refactor and it makes for easier parsing."
                            strCards <- getLine
                            toDiscard <- return $ readCards (words strCards) 3
                            return toDiscard

    readCards :: [String] -> Int -> [Card]
    readCards _ 0 = []
    readCards [] _ = []
    readCards (x:xs) n = Card (read x :: (CardVal, CardCol)) : readCards xs (n-1)

    exchangeCards :: Hand -> [Card] -> Deck -> (Hand, Deck)
    exchangeCards hand [] deck = (hand,deck)
    exchangeCards hand cards deck = case dealNCards (length cards) deck of
                                        (x,y) -> (x ++ [z | z <- hand, ((z `elem` cards) /= True)], y)

    checkResult :: Hand -> Hand -> IO()
    checkResult phand chand = case whoWin phand chand of
                                Lose -> do putStrLn "You lost :("
                                Draw -> do putStrLn "Try harder, draw :("
                                Win -> do putStrLn "Party!"






