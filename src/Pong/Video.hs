{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Pong.Video
    ( draw

    , Color
    ) where

import Pong.Game

import Clash.Prelude hiding (lift)
import RetroClash.Utils
import Data.Word

type Color = (Word8, Word8, Word8)

draw :: Params -> St -> Index ScreenWidth -> Index ScreenHeight -> Color
draw MkParams{..} MkSt{..} ix iy
    | isWall = white
    | isPaddle = blue
    | isBall = yellow
    | otherwise = if _gameOver then red else gray
  where
    x = fromIntegral ix
    y = fromIntegral iy

    rect (x0, y0) (w, h) =
        x `between` (x0, x0 + w) &&
        y `between` (y0, y0 + h)

    isWall = x < wallSize || y < wallSize || y >= (snatToNum (SNat @ScreenHeight) - wallSize)

    paddleStart = snatToNum (SNat @ScreenWidth) - paddleWidth
    isPaddle = rect (paddleStart, _paddleY) (paddleWidth, paddleHeight)

    (ballX, _) = _ballH
    (ballY, _) = _ballV
    isBall = rect (ballX, ballY) (ballSize, ballSize)

white, blue, yellow, red, gray :: Color
white = (0xff, 0xff, 0xff)
blue = (0x40, 0x80, 0xf0)
yellow = (0xf0, 0xe0, 0x40)
red = (0x80, 0x00, 0x00)
gray = (0x30, 0x30, 0x30)
