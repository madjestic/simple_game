module Main where

import Game

main :: IO ()
main = do
  putStrLn "Starting Game"
  runGame
  putStrLn "Exiting Game"
  
