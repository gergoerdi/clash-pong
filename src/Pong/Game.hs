{-# LANGUAGE RecordWildCards, TemplateHaskell, RankNTypes, TypeApplications #-}
module Pong.Game
    ( Params(..)
    , defaultParams

    , InputState(..)
    , St(..)
    , initState
    , updateState

    , ScreenWidth
    , ScreenHeight

    , between
    ) where

import Prelude
import Clash.Prelude hiding (lift)
import RetroClash.Utils

import Data.Word
import Control.Monad.State
import Control.Lens hiding (Index)

type ScreenWidth = 640
type ScreenHeight = 480

screenWidth :: Int
screenWidth = snatToNum (SNat @ScreenWidth)

screenHeight :: Int
screenHeight = snatToNum (SNat @ScreenHeight)

data St = MkSt
    { _ballX, _ballY :: Int
    , _ballSpeedX, _ballSpeedY :: Int
    , _paddleY :: Int
    , _gameOver :: Bool
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''St

initState :: St
initState = MkSt
    { _ballX = 10
    , _ballY = 100
    , _ballSpeedX = 5
    , _ballSpeedY = -7
    , _paddleY = 100
    , _gameOver = False
    }

data Params = MkParams
    { wallSize, ballSize, paddleSize :: Int
    , paddleWidth, paddleSpeed :: Int
    }

data InputState = MkInputState
    { paddleUp :: Bool
    , paddleDown :: Bool
    }

double :: (Num a) => a -> a
double x = x + x

reflect :: (Num a, Num a', Ord a, Ord a') => Lens' s a -> Lens' s a' -> (a, a') -> State s Bool
reflect x x' (p, n) = do
    x0 <- use x
    let over = dir $ p - x0
    if over > 0 then do
        x += dir (double over)
        x' %= negate
        return True
      else return False
  where
    dir = if n > 0 then id else negate

move :: (Num a) => Lens' s a -> Lens' s a -> State s ()
move x dx = do
    dx <- use dx
    x += dx

x `between` (lo, hi) = lo <= x && x <= hi

updateHoriz :: Params -> InputState -> State St ()
updateHoriz MkParams{..} MkInputState{..} = do
    move ballX ballSpeedX
    reflect ballX ballSpeedX (wallSize, 1)
    atPaddle <- gets $ \st@MkSt{..} -> _ballY `between` (_paddleY, _paddleY + paddleSize)
    when atPaddle $ do
        hitPaddle <- reflect ballX ballSpeedX (screenWidth - paddleWidth - ballSize, -1)
        when hitPaddle $ ballSpeedY += nudge
  where
    nudge | paddleDown = 5
          | paddleUp = -5
          | otherwise = 0

updateVert :: Params -> State St ()
updateVert MkParams{..} = void $ do
    move ballY ballSpeedY
    reflect ballY ballSpeedY (wallSize, 1)
    reflect ballY ballSpeedY (screenHeight - wallSize - ballSize, -1)

updatePaddle :: Params -> InputState -> State St ()
updatePaddle MkParams{..} MkInputState{..} = do
    when paddleUp $ paddleY -= paddleSpeed
    when paddleDown $ paddleY += paddleSpeed
    paddleY %= clamp (wallSize, screenHeight - (wallSize + paddleSize))

clamp :: (Ord a) => (a, a) -> a -> a
clamp (lo, hi) = max lo . min hi

updateState :: Params -> InputState -> St -> St
updateState params inp@MkInputState{..} = execState $ do
    gameOver .= False
    updateVert params
    updateHoriz params inp
    updatePaddle params inp
    outOfBounds <- gets $ \st@MkSt{..} -> _ballX > screenWidth
    when outOfBounds $ do
        gameOver .= True
        ballX .= screenWidth `shiftR` 1

defaultParams :: Params
defaultParams = MkParams
    { wallSize = 5
    , ballSize = 10
    , paddleSize = 150
    , paddleWidth = 15
    , paddleSpeed = 5
    }
