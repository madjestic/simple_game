module GameState where

import Control.Monad.State as State

data GameState =
     GameState
     {
       tick      :: Integer
     , increment :: Integer
     , quitGame    :: Bool
     } deriving Show

defaultGame :: GameState
defaultGame =
     GameState
     {
       tick      = 0 
     , increment = 0
     , quitGame    = False
     } 

defaultGameState :: StateT GameState IO ()
defaultGameState = do
  return ()
