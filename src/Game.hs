{-# LANGUAGE FlexibleContexts #-}

module Game where

import Control.Monad.State as State
import SDL hiding (get)

import GameState
import Input

inc :: Integer -> GameState -> GameState
inc k (GameState c _ q) =
  GameState
  { tick      = c + k
  , increment = k
  , quitGame  = q
  }

loop :: Renderer -> StateT GameState IO ()
loop renderer = do
  liftIO $ delay 100
  quitGame <- handleEvents
  -- game0 <- get
  -- game1 <- get
  -- modify $ inc (increment game1)
  get >>= (liftIO . print)
  -- liftIO $ print game0
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  --unless qPressed (appLoop renderer)
  
  unless quitGame $ loop renderer

runGame :: Renderer -> IO ()
runGame r = do
  evalStateT (do
      defaultGameState
      loop r)
      defaultGame 
