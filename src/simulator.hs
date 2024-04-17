{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import Pong.Game
import Pong.Video
import RetroClash.Sim.SDL
import Control.Monad
import Control.Monad.State

main :: IO ()
main =
    flip evalStateT initState $
    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let params = defaultParams
        modify $ updateState params $ MkInputs
            { paddleUp = keyDown ScancodeUp
            , paddleDown = keyDown ScancodeDown
            }
        gets $ rasterizePattern . draw params
  where
    videoParams = MkVideoParams
        { windowTitle = "Pong"
        , screenScale = 4
        , screenRefreshRate = 60
        , reportFPS = True
        }
