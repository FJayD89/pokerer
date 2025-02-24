import Helpers
import Hand
import Card   
   

     


-- hasRFlush :: [Card] -> Bool

 

handRF = [Card Ten Spades, Card King Spades, Card Jack Spades, Card Ace Spades, Card Queen Spades]
handSF		= map (\f -> Card f Spades) fs
hand4oak	= (map (Card King) (fullRange :: [Suit])) ++ []
handFH		= map card_tuple (cart [King, Two, Seven] [Diamonds, Hearts, Clubs])
handStraight	= map card_tuple (zip fs ss)
handFlush	= map (card_tuple . (,Spades)) [Two, Six, Ace, Five, Jack] 
hand3oak	= map (card_tuple . (Ten, )) [Diamonds, Hearts, Clubs] ++ [Card Five Clubs]
hand2p		= map card_tuple (cart [Ten, Jack] [Spades, Diamonds]) 
handPair	= (Card Four Clubs) : (tail $ map card_tuple (zip fs ss))


ss = [Spades, Spades, Diamonds, Clubs, Spades]
fs = [Five, Three, Four, Seven, Six]



