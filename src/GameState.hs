module GameState where

import Control.Monad.State as State

data GameState =
     GameState
     {
       tick      :: Integer
     , increment :: Integer
     } deriving Show

defaultGame :: GameState
defaultGame =
     GameState
     {
       tick      = 0 
     , increment = 1 } 

defaultGameState :: StateT GameState IO ()
defaultGameState = do
  return ()
