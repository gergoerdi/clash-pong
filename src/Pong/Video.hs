{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Pong.Video
    ( draw

    , Color
    ) where

import Pong.Game

import Prelude
import Clash.Prelude hiding (lift)
import RetroClash.Utils
import Data.Word

type Color = (Word8, Word8, Word8)

black :: Color
black = (0x00, 0x00, 0x00)

white :: Color
white = (0xff, 0xff, 0xff)

blue :: Color
blue = (0x40, 0x80, 0xf0)

yellow :: Color
yellow = (0xf0, 0xe0, 0x40)

red :: Color
red = (0x80, 0x00, 0x00)

gray :: Color
gray = (0x30, 0x30, 0x30)

draw :: Params -> St -> Index ScreenWidth -> Index ScreenHeight -> Color
draw MkParams{..} MkSt{..} ix iy
    | isWall = white
    | isPaddle = blue
    | isBall = yellow
    | otherwise = backColor
  where
    (ballX, _) = _ballH
    (ballY, _) = _ballV

    x = fromIntegral ix
    y = fromIntegral iy

    isWall = x < wallSize || y < wallSize || y >= (snatToNum (SNat @ScreenHeight) - wallSize)

    paddleStart = snatToNum (SNat @ScreenWidth) - paddleWidth

    rect (x0, y0) (w, h) = x `between` (x0, x0 + w) && y `between` (y0, y0 + h)

    isPaddle = rect (paddleStart, _paddleY) (paddleWidth, paddleHeight)
    isBall = rect (ballX, ballY) (ballSize, ballSize)

    paddleColor = (0x40, 0x80, 0xf0)
    ballColor = (0xf0, 0xe0, 0x40)
    wallColor = white
    backColor = if _gameOver then red else gray
