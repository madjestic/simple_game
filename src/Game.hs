{-# LANGUAGE FlexibleContexts #-}

module Game where

import Control.Monad.State as State
import SDL.Time (delay)

import GameState
import Input


defaultState :: StateT GameState IO ()
defaultState = do
  return ()

inc :: GameState -> Integer -> GameState
inc (GameState c) n = GameState ( c + n )

-- input :: (Monad IO) => StateT GameState IO Integer
-- input = do
--   x <- getLine
--   return

loop :: StateT GameState IO GameState
loop = do
  liftIO $ delay 100
  state <- get
  handleEvents
  modify $ flip inc 1
  liftIO $ print state
  loop

startState :: GameState
startState = GameState { tick = 0 }

runGame :: IO GameState
runGame = do
  evalStateT (do
      defaultState
      loop)
      startState
