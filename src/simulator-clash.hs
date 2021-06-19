{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Clash.Prelude

import Pong
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL
import Control.Monad.State
import Control.Monad.Loops

board' (up, dn) =
    let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen up dn
    in bundle (vgaHSync, vgaVSync, bitCoerce <$> bundle (vgaR, vgaG, vgaB))

main :: IO ()
main = do
    buf <- newBufferArray

    sim <- simulateIO (board' . unbundle) (toActive False, toActive False)

    flip evalStateT initSink $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let up = toActive $ keyDown ScancodeUp
            down = toActive $ keyDown ScancodeDown

        untilM_ (return ()) $ do
            sim $ \vgaOut -> do
                frameDone <- vgaSinkBuf vga640x480at60 buf vgaOut
                return ((up, down), frameDone)

        return $ rasterizeBuffer buf
  where
    videoParams = MkVideoParams
        { windowTitle = "Pong"
        , screenScale = 2
        , screenRefreshRate = 60
        , reportFPS = True
        }
