{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import Pong.Game
import Pong.Video
import RetroClash.Sim.SDL

import SDL hiding (get)
import Control.Monad.State

main :: IO ()
main = flip evalStateT initState $ withMainWindow "Pong" 4 $ \events keyState -> do
    modify $ updateState defaultParams $ MkInputs
        { paddleUp = keyState ScancodeUp
        , paddleDown = keyState ScancodeDown
        }
    gets $ Just . rasterizePattern . draw defaultParams
