{-# LANGUAGE TupleSections #-}

module Main where

import System.Environment

import Data.Char

import qualified Data.Set as Set
import qualified Data.Map as Map

wordMap s =
    (
        Map.fromSet (\c -> (length . filter (==c)) s) .
        Set.fromList .
        fmap toLower
        ) s

matches m w = Map.isSubmapOfBy (<=) m (wordMap w)

main :: IO ()
main = do
    args <- getArgs
    dict <- readFile "dictionary.txt"
    
    let wds = fmap (\s -> (s, wordMap s)) (lines dict)
    let ms = filter (\(_, m) -> matches m (fmap toLower (head (args)))) wds

    putStrLn (unlines (map fst ms))