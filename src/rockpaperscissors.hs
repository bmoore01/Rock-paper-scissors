import System.Random
import Data.Char

processInput :: Char -> String
processInput 'r' = "Rock"
processInput 'R' = "Rock"
processInput '1' = "Rock"
processInput 'p' = "Paper"
processInput 'P' = "Paper"
processInput '2' = "Paper"
processInput 's' = "Scissors"
processInput 'S' = "Scissors"
processInput '3' = "Scissors"

inputToInt :: String -> Int
inputToInt "Rock" = 1
inputToInt "Paper" = 2
inputToInt "Scissors" = 3

calculateWinner :: String -> Int -> String
calculateWinner user computer 
    | user_int == computer = "It's a draw!"
    | user_int == 1 && computer == 3 = "You win!"
    | user_int == 1 && computer == 2 = "You lose!"
    | user_int == 2 && computer == 1 = "You win!"
    | user_int == 2 && computer == 3 = "You lose!"
    | user_int == 3 && computer == 2 = "You win!"
    | user_int == 3 && computer == 1 = "You lose!"
    where user_int = inputToInt (processInput (head user))

getRand :: IO Int
getRand = getStdRandom (randomR (1,3))

main = do
    putStrLn "Welcome to rock, paper, scissors"
    putStrLn "What would you like to play?"
    putStrLn "1) Rock"
    putStrLn "2) Paper"
    putStrLn "3) Scissors"
    line <- getLine
    rand <- getRand
    if null line 
        then return ()
    else do 
        print "You have entered"
        putStrLn $ processInput (head line)
        print "The computer played"
        putStrLn $ processInput (intToDigit rand)
        print (calculateWinner line rand)
