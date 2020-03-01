{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import Pong.Game
import Pong.Video
import RetroClash.Sim.SDL
import Control.Monad.State

main :: IO ()
main = flip evalStateT initState $ withMainWindow videoParams $ \events keyDown -> do
    when (keyDown ScancodeEscape) mzero

    modify $ updateState defaultParams $ MkInputs
        { paddleUp = keyDown ScancodeUp
        , paddleDown = keyDown ScancodeDown
        }
    gets $ rasterizePattern . draw defaultParams
  where
    videoParams = MkVideoParams
        { windowTitle = "Pong"
        , screenScale = 4
        , screenRefreshRate = 60
        }
