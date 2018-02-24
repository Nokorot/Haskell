
import Data.Time.Clock.POSIX
import Data.List
import Data.List.Split

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = someFunction

someFunction input = do
  let ls = lines input
  intercalate "\n" (map multyply ls)
  --[ a + b | (a, b) <- (splitOn " " g) | g <- ls ]

readl [] = []
readl (x:xs) = (read x :: Int) : readl xs

mult [] = 1
mult (x:xs) = x * (mult xs)

multyply x = do
  let es = splitOn " " x
  show (mult (readl es))

a `plus` b = a + b

data a `Pair` b = a `Pair` b
                  deriving (Show)

expand a 
    | type a == type [a] = head a
    | otherwise = a
