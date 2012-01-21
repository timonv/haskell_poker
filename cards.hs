module Cards where
    import Data.Maybe -- fromJust
    import Data.Tuple -- swap

    showCardVal = [(Two,"Two"), (Three,"Three"),(Four,"Four"),(Five,"Five"),(Six,"Six"),(Seven,"Seven"),(Eight,"Eight"),(Nine,"Nine"),(Ten,"Ten"),(Jack,"Jack"),(Queen,"Queen"),(King,"King"),(Ace, "Ace")]
    showCardCol = [(Spades, "Spades"), (Clubs, "Clubs"), (Diamonds, "Diamonds"), (Hearts, "Hearts")]

    -- I don't feel like defining 52 constants.
    data CardCol = Spades | Clubs | Diamonds | Hearts deriving (Eq, Show, Bounded, Enum, Read)
    data CardVal = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq, Enum, Show, Bounded, Read)
    data Card = Card(CardVal, CardCol) deriving (Read)

    -- We use this for VALUE checking
    instance Eq Card where
        Card(x,y) == Card(a,b) = x == a

    -- We use this for VALUE checking
    instance Ord Card where
        Card(x,y) > Card(a,b) =  x >  a
        Card(x,y) < Card(a,b) =  x <  a
        Card(x,y) <= Card(a,b) =  x <  a ||  x ==  a
        Card(x,y) >= Card(a,b) =  x >  a ||  x ==  a

    instance Show Card where
        show (Card(x,y)) = (fromJust $ lookup x showCardVal) ++ " of " ++ (fromJust $ lookup y showCardCol)

