module Input(handleEvents)
where


import Control.Monad.State as State
import SDL
import SDL.Raw.Event-- import Data.List hiding (maximum)
-- import Data.Foldable
-- import Control.Monad hiding (forM_)
-- import Control.Monad.State as State hiding (forM_)
-- import Prelude hiding (catch, until, maximum)

import GameState

import Debug.Trace as DT

inputMapping
  :: [(Scancode,
       (StateT GameState IO (),
        StateT GameState IO ()))]
inputMapping =
  [
    --(ScancodeW, (inc 1, inc 0))
    (ScancodeW, (inc (DT.trace ("suka") 1), inc 0))
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
  , quitGame    = q
  }

processEvent :: (Monad m) => [(Scancode , (m (), m ()))] -> Event -> m ()
processEvent n e =
  let mk = case eventPayload e of
             KeyboardEvent keyboardEvent -> Just
               ( keyboardEventKeyMotion keyboardEvent == Pressed
               , keysymScancode (keyboardEventKeysym keyboardEvent))
             _                      -> Nothing
  in case (DT.trace ("ebat") $ mk) of
       Nothing     -> return ()
       Just (e, k) -> case lookup k n of
                        Nothing       -> return ()
                        Just (a1, a2) -> if e then a1 else a2

processEvents :: (Monad m) => [(Scancode, (m (), m ()))] -> [Event] -> m ()
processEvents ns = mapM_ (processEvent ns)

isQuit :: EventPayload -> Bool
isQuit ev =
  case ev of
    QuitEvent -> True
    _         -> False

-- isQuit :: [SDL.Event] -> Bool
-- isQuit =
--   case (hasEvent QuitEvent) of
--     True -> True
--     _    -> False
  -- where isq QuitEvent = True
  --       -- isq (KeyboardEvent keyboardEvent) = True
  --       isq _ = False

handleEvents :: StateT GameState IO Bool
handleEvents = do
  events <- SDL.pollEvents
  processEvents inputMapping events
  -- liftIO $ print "ebat"
  return $ any isQuit $ fmap eventPayload events
-- handleEvents = do
--   events <- liftIO $ pollAllSDLEvents
--   processEvents inputMapping events
--   return $ isQuit events


-- inputMapping = 
--   [ (SDLK_w,     (accelerate 0.002,    accelerate 0))
--   , (SDLK_s,     (accelerate (-0.002), accelerate 0))
--   , (SDLK_a,     (turn 1.5, setTurn 0))
--   , (SDLK_d,     (turn (-1.5), setTurn 0))
--   , (SDLK_UP,    (accelerate 0.002, accelerate 0))
--   , (SDLK_DOWN,  (accelerate (-0.002), accelerate 0))
--   , (SDLK_LEFT,  (turn 1.5, setTurn 0))
--   , (SDLK_RIGHT, (turn (-1.5), setTurn 0))
--   , (SDLK_MINUS, (changeZoom zoomChangeFactor, setZoomDelta 0))
--   , (SDLK_PLUS,  (changeZoom (-zoomChangeFactor), setZoomDelta 0))
--   , (SDLK_i,     (showInfo, return ()))
--   , (SDLK_p,     (modify $ modStopped not, return ()))
--   , (SDLK_m,     (showMap, return ()))
--   ]

-- showMap :: StateT GameState IO ()
-- showMap = do
--   state <- State.get
--   let maxrad = maxDistance $ aobjects state
--   let ((minx', maxx'), (miny', maxy')) = boxThatIncludes (-maxrad, maxrad) (-maxrad, maxrad) 10 10 width height
--   let objrad = 0.01 * (min (maxx' - minx') (maxy' - miny'))
--   liftIO $ until anyKeyOrMouseWasPressedIO $ 
--     inOrthoBoxDraw minx' maxx' miny' maxy' (-10) 10 $ do
--       delay 10
--       drawGLScreen (Just objrad) [tri state] (aobjects state)

-- maxDistance :: AObjTree -> GLdouble
-- maxDistance s = go 0 s
--   where go disp (Leaf aobj)      = disp + orbitRadius aobj
--         go disp (Node (_, r) ts) = maximum $ map (go (disp + r)) ts

-- showInfo = do
--   s <- State.get
--   liftIO . putStrLn $ "Zoom: " ++ show (camzoom $ camstate s)
--   liftIO . putStrLn $ "Player position: " ++ show (Entity.position $ tri s)
--   forM_ (aobjects s) $ \aobj -> do
--     liftIO . putStrLn $ "Astronomical body position: " ++ show (AObject.getPosition aobj)
--   liftIO . putStrLn $ show $ allegattitudes s
