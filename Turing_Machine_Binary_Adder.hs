--This is the Binary Adder Turing Machine for problem 11.2c
-- we can test this program , for example, by calling run "$0100$0010$", and we get "$0000$0110$."
import Prelude hiding (head)

data State = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 deriving (Show) 
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
    | head tape '0' = delta S1 $ right $ write tape '1' 
    | head tape '1' = delta S1 $ right $ write tape '0'
    | head tape '$' = delta S2 $ left $ write tape '$' 
delta S2 tape 
    | head tape '0' = delta S3 $ left $ write tape '1' 
    | head tape '1' = delta S2 $ left $ write tape '0'  
delta S3 tape
    | head tape '0' = delta S3 $ left $ write tape '0'
    | head tape '1' = delta S3 $ left $ write tape '1' 
    | head tape '$' = delta S4 $ right $ write tape '$'
delta S4 tape
    | head tape '0' = delta S4 $ right $ write tape '1'
    | head tape '1' = delta S4 $ right $ write tape '0' 
    | head tape '$' = delta S5 $ right $ write tape '$'
delta S5 tape 
    | head tape '0' = delta S5 $ right $ write tape '0' 
    | head tape '1' = delta S5 $ right $ write tape '1'
    | head tape '$' = delta S6 $ left $ write tape '$' 
delta S6 tape 
    | head tape '0' = delta S7 $ left $ write tape '1' 
    | head tape '1' = delta S6 $ left $ write tape '0'  
delta S7 tape
    | head tape '0' = delta S7 $ left $ write tape '0' 
    | head tape '1' = delta S7 $ left $ write tape '1'
    | head tape '$' = delta S8 $ left $ write tape '$'
delta S8 tape
    | head tape '0' = delta S8 $ left $ write tape '0' 
    | head tape '1' = delta S9 $ left $ write tape '1'
    | head tape '$' = delta S10 $ right $ write tape '$'
delta S9 tape
    | head tape '0' = delta S9 $ left $ write tape '0' 
    | head tape '1' = delta S9 $ left $ write tape '1'
    | head tape '$' = delta S1 $ right $ write tape '$'
delta S10 tape = tape

run :: String -> String
run xs = content (delta S0 (Tape xs 0))