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

data Request = Yes | No | Get Int deriving (Eq, Show)

parse :: String -> [Request]
parse = catMaybes . map p . lines
  where
    p "y" = Just Yes
    p "Y" = Just Yes
    p "n" = Just No
    p "N" = Just No
    p s = fmap Get $ safeRead s

data Response  = Welcome Int | Start | Win | Lose | Turn Int Int | Invalid Int deriving Show

pprint :: [Response] -> String
pprint = unlines . map p
  where
    p :: Response -> String
    p (Welcome n) = "The rest is " ++ show n ++ "stones. You can get 1-3 stones. Your turn."
    p Start       = "Let's start a new game. You can get 1-3 stones. Your turn."
    p Win         = "You win! Continue? (y/n)"
    p Lose        = "You lose! Continue? (y/n)"
    p (Turn x n)  = "I got " ++ show x ++ ". then the rest is " ++ show n ++ ". Your turn."
    p (Invalid n) = " You can only get up to " ++ show n ++ " stones. Your turn."

data State = Play Int StdGen | Continue StdGen

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
    psi (_, [])             = Nothing
    psi (Continue g, c:cs)
      | c == Yes = Just (Start, (Play stones g, cs))
      | otherwise           = Nothing
    psi (Play n g, x:xs)
      | y `notElem` [1,2,3] = Just (Invalid 3, (Play n g, xs))
      | y > n               = Just (Invalid n, (Play n g, xs))
      | m == 1              = Just (Win, (Continue g, xs))
      | m == 0              = Just (Lose, (Continue g, xs))
      | otherwise           = Just (Turn z' (m-z'), (Play (m-z') g', xs))
      where y = case x of
              Get i -> i
              _     -> error "Unexpected input"
            m  = n - y
            r  = m `mod` 4
            z = (r-1) `mod` 4
            (z', g')
              | z /= 0    = (z, g)
              | otherwise = randomR (1, min 3 m) g
