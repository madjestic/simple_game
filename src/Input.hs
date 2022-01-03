module Input(handleEvents)
where

import Control.Monad.State as State
import SDL
import SDL.Raw.Event-- import Data.List hiding (maximum)

import GameState

import Debug.Trace as DT

inputMapping
  :: [(Scancode,
       (StateT GameState IO (),
        StateT GameState IO ()))]
inputMapping =
  [
    (ScancodeW, (inc   10 , inc 0))
  , (ScancodeS, (inc (-10), inc 0))
  , (ScancodeQ, (exit True, exit False))
  ]

exit :: Bool -> StateT GameState IO ()
exit b = modify $ quit' b

quit' :: Bool -> GameState -> GameState
quit' b g = g { quitGame = b}

inc :: Integer -> StateT GameState IO ()
inc n = modify $ inc' n

inc' :: Integer -> GameState -> GameState
inc' k (GameState c _ q) =
  GameState
  { tick      = c + k
  , increment = k
  , quitGame  = q
  }

processEvent :: (Monad m) => [(Scancode , (m (), m ()))] -> Event -> m ()
processEvent n e =
  let mk = case eventPayload e of
             KeyboardEvent keyboardEvent -> Just
               ( keyboardEventKeyMotion keyboardEvent == Pressed
               , keysymScancode (keyboardEventKeysym keyboardEvent))
             _ -> Nothing
  in case mk of
       Nothing     -> return ()
       Just (e, k) -> case lookup k n of
                        Nothing       -> return ()
                        Just (a1, a2) -> if e then a1 else a2

processEvents :: (Monad m) => [(Scancode, (m (), m ()))] -> [Event] -> m ()
processEvents ns = mapM_ (processEvent ns)

isQuit :: EventPayload -> Bool
isQuit ev =
  case ev of
    KeyboardEvent keyboardEvent -> 
      keyboardEventKeyMotion keyboardEvent == Pressed
      && keysymScancode (keyboardEventKeysym keyboardEvent) == ScancodeQ
    QuitEvent -> True
    _         -> False

handleEvents :: StateT GameState IO Bool
handleEvents = do
  events <- SDL.pollEvents
  processEvents inputMapping events
  return $ any isQuit $ fmap eventPayload events
