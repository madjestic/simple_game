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
  get >>= (liftIO . print)
  GameState t _ _ <- get
  rendererDrawColor renderer $= V4 (fromIntegral t) 0 255 255
  clear renderer
  present renderer
  unless quitGame $ loop renderer

defaultGameState :: StateT GameState IO ()
defaultGameState = do
  return ()

runGame :: Renderer -> IO ()
runGame r = do
  evalStateT (do
      defaultGameState
      loop r)
      defaultGame 
