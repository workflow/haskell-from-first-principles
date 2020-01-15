module Reverse where

rvrs :: String -> String
rvrs x = let
  curry = take 5 x
  is = take 2 (drop 6 x)
  awesome = drop 9 x
  in awesome ++ " " ++ is ++ " " ++ curry

main :: IO ()
main = print $ rvrs "Curry is awesome"