module Main where

import GameState.Game

main :: IO ()
main = do
  putStrLn "Starting Game"
  runGame
  putStrLn "Exiting Game"
  
