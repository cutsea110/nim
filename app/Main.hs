module Main where

import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)
import System.Random (getStdGen, StdGen, randomR)

safeRead :: Read a => String -> Maybe a
safeRead s =
  case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

data Request = Yes | No | Get Int | Illegal String deriving (Eq, Show)

parse :: String -> [Request]
parse = catMaybes . map p . lines
  where
    p "y" = Just Yes
    p "Y" = Just Yes
    p "n" = Just No
    p "N" = Just No
    p s = case safeRead s of
      Just i  -> Just (Get i)
      Nothing -> Just (Illegal s)

data Response  = Welcome Int | Start | Win | Lose | Turn Int Int | Invalid Int | Error State deriving Show

pprint :: [Response] -> String
pprint = unlines . map p
  where
    p :: Response -> String
    p (Welcome n) = "The rest is " ++ show n ++ "stones. You can get 1-3 stones. Your turn."
    p Start       = "Let's start a new game. You can get 1-3 stones. Your turn."
    p Win         = "You win! Continue? (y/n)"
    p Lose        = "You lose! Continue? (y/n)"
    p (Turn x n)  = "I got " ++ show x ++ ". then the rest is " ++ show n ++ ". Your turn."
    p (Invalid n) = "You can only get up to " ++ show n ++ " stones. Your turn."
    p (Error s)   = "Oops! Invalid input. " ++ again
      where again = case s of
              Play n _ -> "The rest is " ++ show n ++ " stones. You can get 1-3 stones. Your turn."
              Continue _ -> "Continue? (y/n)"

data State = Play Int StdGen | Continue StdGen deriving Show

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout NoBuffering
  gen <- getStdGen
  interact (pprint . player gen 30 . parse)
  putStrLn "Thanks for playing!"

player :: StdGen -> Int -> [Request] -> [Response]
player gen stones inputs = Welcome stones : unfoldr psi (Play stones gen, inputs)
  where
    psi :: ((State, [Request]) -> Maybe (Response, (State, [Request])))
    psi (_, [])               = Nothing
    psi (state, Illegal _:xs) = Just (Error state, (state, xs))
    psi (Continue g, c:cs)
      | c == Yes              = Just (Start, (Play stones g, cs))
      | c == No               = Nothing
      | otherwise             = Just (Error (Continue g), (Continue g, cs))
    psi (Play n g, Get y:xs)
      | y `notElem` [1,2,3]   = Just (Invalid 3, (Play n g, xs))
      | y > n                 = Just (Invalid n, (Play n g, xs))
      | m == 1                = Just (Win, (Continue g, xs))
      | m == 0                = Just (Lose, (Continue g, xs))
      | otherwise             = Just (Turn z' (m-z'), (Play (m-z') g', xs))
      where m  = n - y
            r  = m `mod` 4
            z = (r-1) `mod` 4
            (z', g') | z /= 0    = (z, g)
                     | otherwise = randomR (1, min 3 m) g
    psi (Play n g, _:xs)      = Just (Error (Play n g), (Play n g, xs))
