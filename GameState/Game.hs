{-# LANGUAGE FlexibleContexts #-}

module GameState.Game where

import Control.Monad.State as State
import Control.Monad.Reader
import Control.Monad.Writer

import SDL.Time (delay)

data GameState =
     GameState
     {
       tick :: Integer
     } deriving Show

initState :: StateT GameState IO ()
initState = do
  return ()

incRSW :: (MonadReader Integer m, MonadState GameState m, MonadWriter [Integer] m) => m ()
--incRSW = ask >>= modify . flip inc >> get >>= tell . (:[]) . tick
incRSW = get >>= tell . (:[]) . tick

loop :: StateT GameState IO GameState
loop = do
  liftIO $ delay 10
  liftIO $ print "Awaiting user input:"
  liftIO $ getLine >>= (\x -> putStrLn $ "It's :" ++ show x)
  loop

startState :: GameState
startState = GameState { tick = 0 }

runGame :: IO GameState
runGame = do
  evalStateT (do
      initState
      loop)
      startState
