module Explode where

myWords :: String -> [String]
myWords "" = []
myWords s = [takeWhile (/=' ') s] ++ myWords remainder where
                remainder = dropWhile (==' ') . dropWhile (/=' ') $ s