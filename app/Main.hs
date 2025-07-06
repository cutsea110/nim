module Main where

import Data.List (unfoldr)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)
import System.Random (getStdGen, StdGen, randomR)

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  gen <- getStdGen
  interact (unlines . player gen 30 . lines)
  putStrLn "Thanks for playing!"

data State = Play Int StdGen | Continue StdGen

player :: StdGen -> Int -> [String] -> [String]
player gen stones inputs = firstMessage : unfoldr psi (Play stones gen, inputs)
  where
    firstMessage = "The rest is " ++ show stones ++ ". You can get 1-3 stones. Your turn."
    
    psi :: ((State, [String]) -> Maybe (String, (State, [String])))
    psi (_, [])             = Nothing
    psi (Continue g, c:cs)
      | c `elem` ["y", "Y"] = Just ("Start new game. You can get 1-3 stones. Your turn.", (Play stones g, cs))
      | otherwise           = Nothing
    psi (Play n g, x:xs)
      | y `notElem` [1,2,3] = Just ("Invalid input. You can get 1-3 stones. Your turn.", (Play n g, xs))
      | y > n               = Just ("You can only get up to " ++ show n ++ " stones. Your turn.", (Play n g, xs))
      | m == 1              = Just ("You win! Continue? (y/n)", (Continue g, xs))
      | m == 0              = Just ("You lose! Continue? (y/n)", (Continue g, xs))
      | otherwise           = Just ("I got " ++ show z' ++ ", then the rest is " ++ show (m-z') ++ ". Your turn.", (Play (m-z') g', xs))
      where y = read x :: Int
            m  = n - y
            r  = m `mod` 4
            z = (r-1) `mod` 4
            (z', g')
              | z /= 0    = (z, g)
              | otherwise = randomR (1, min 3 m) g
