{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Pong.Game
    ( Params(..)
    , defaultParams

    , InputState(..)
    , St(..)
    , initState
    , updateState
    , draw

    , Color
    , Draw
    , screenWidth
    , screenHeight
    ) where

import Prelude
import Clash.Prelude hiding (lift)
import RetroClash.Utils

import Data.Word
import Control.Monad.State
import Control.Lens hiding (Index)

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

type Color = (Word8, Word8, Word8)

black :: Color
black = (0x00, 0x00, 0x00)

white :: Color
white = (0xff, 0xff, 0xff)

type Draw w h = (Index w, Index h) -> Color

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

draw :: Params -> St -> Draw 640 480
draw MkParams{..} MkSt{..} (x0, y0)
    | isWall = wallColor
    | isPaddle = paddleColor
    | isBall = ballColor
    | otherwise = backColor
  where
    x = fromIntegral x0
    y = fromIntegral y0

    isWall = x < wallSize || y < wallSize || y > (screenHeight - wallSize)

    paddleStart = screenWidth - paddleWidth

    rect (x0, y0) (w, h) x y = x `between` (x0, x0 + w) && y `between` (y0, y0 + h)

    isPaddle = rect (paddleStart, _paddleY) (paddleWidth, paddleSize) x y
    isBall = rect (_ballX, _ballY) (ballSize, ballSize) x y

    paddleColor = (0x40, 0x80, 0xf0)
    ballColor = (0xf0, 0xe0, 0x40)
    wallColor = white
    backColor = if _gameOver then (0x80, 0x00, 0x00) else (0x30, 0x30, 0x30)

defaultParams :: Params
defaultParams = MkParams
    { wallSize = 5
    , ballSize = 10
    , paddleSize = 150
    , paddleWidth = 15
    , paddleSpeed = 5
    }
