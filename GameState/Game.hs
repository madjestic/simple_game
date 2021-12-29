module GameState.Game where

import Control.Monad.State as State
import SDL.Time (delay)

data GameState =
     GameState
     {
       tick :: Integer
     } deriving Show

initState :: StateT GameState IO ()
initState = do
  return ()

loop :: StateT GameState IO GameState
loop = do
  liftIO $ delay 10
  liftIO $ print  $ "Awaiting user input:"
  liftIO $ getLine >>= print
  loop

startState :: GameState
startState = GameState { tick = 0 }

runGame :: IO GameState
runGame = do
  evalStateT (do
      initState
      loop)
      startState
