--This is the Increment Turing Machine for problem 11.2a
-- we can test this program , for example, by calling run "$0101$", and we get "$0110$."
import Prelude hiding (head)

data State = S0 | S1 | S2 | S3 deriving (Show) 
data Tape = Tape String Int deriving (Show)

head :: Tape -> Char -> Bool 
head (Tape xs i) c = xs !! i == c

content :: Tape -> String 
content (Tape xs _) = xs

left :: Tape -> Tape 
left (Tape xs i)
    | i == 0 = Tape ("0" ++ xs) 0 
    | otherwise = Tape xs (i - 1)

right :: Tape -> Tape 
right (Tape xs i)
    | i + 1 == length xs = Tape (xs ++ "0") (i + 1) 
    | otherwise = Tape xs (i + 1)

write :: Tape -> Char -> Tape 
write (Tape xs i) c = Tape (take i xs ++ [c] ++ drop (i + 1) xs) i

delta :: State -> Tape -> Tape 
delta S0 tape
    | head tape '$' = delta S1 $ right $ write tape '$'
delta S1 tape 
    | head tape '0' = delta S1 $ right $ write tape '0' 
    | head tape '1' = delta S1 $ right $ write tape '1'
    | head tape '$' = delta S2 $ left $ write tape '$' 
delta S2 tape 
    | head tape '0' = delta S3 $ left $ write tape '1' 
    | head tape '1' = delta S2 $ left $ write tape '0'  
delta S3 tape = tape 
    
run :: String -> String
run xs = content (delta S0 (Tape xs 0))