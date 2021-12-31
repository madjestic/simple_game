{-# LANGUAGE FlexibleContexts #-}

module Game where

import Control.Monad.State as State
import SDL.Time (delay)

import GameState
import Input

inc :: GameState -> GameState
inc (GameState c n) =
  GameState
  { tick      = c + n
  , increment = n
  }

inc' :: Integer -> GameState -> GameState
inc' k (GameState c _) =
  GameState
  { tick      = c + k
  , increment = k
  }

loop :: StateT GameState IO GameState
loop = do
  liftIO $ delay 100
  state <- get
  handleEvents
  --modify inc
  modify $ inc' 1
  liftIO $ print state
  loop

startState :: GameState
startState =
  GameState { tick = 0
            , increment = 1 }

runGame :: IO GameState
runGame = do
  evalStateT (do
      defaultGameState
      loop)
      startState
