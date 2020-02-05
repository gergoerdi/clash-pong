{-# LANGUAGE RecordWildCards #-}
module Pong.SDL (withMainWindow) where

import Prelude
import Clash.Prelude hiding (lift)
import RetroClash.Utils

import Pong.Game

import SDL hiding (get)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Control.Monad

screenScale :: CInt
screenScale = 2

screenRefreshRate :: Word32
screenRefreshRate = 60

scanRaster :: Ptr Word8 -> CInt -> Draw 640 480 -> IO ()
scanRaster ptr stride draw = do
    forM_ [minBound..maxBound] $ \y -> do
        let base = fromIntegral y * fromIntegral stride
        forM_ [minBound .. maxBound] $ \x -> do
            let offset = base + (fromIntegral x * 4)
            let (r, g, b) = draw (x, y)
            pokeElemOff ptr (offset + 0) maxBound
            pokeElemOff ptr (offset + 1) b
            pokeElemOff ptr (offset + 2) g
            pokeElemOff ptr (offset + 3) r

withMainWindow :: Text -> s -> ([Event] -> (Scancode -> Bool) -> s -> IO (Maybe (Draw 640 480, s))) -> IO ()
withMainWindow title s0 runFrame = do
    initializeAll
    window <- createWindow title defaultWindow
    let screenSize = fromIntegral <$> V2 screenWidth screenHeight
    windowSize window $= fmap (screenScale *) screenSize

    renderer <- createRenderer window (-1) defaultRenderer
    texture <- createTexture renderer RGBA8888 TextureAccessStreaming screenSize
    let render draw = do
            (ptr, stride) <- lockTexture texture Nothing
            let ptr' = castPtr ptr
            scanRaster ptr' stride draw
            unlockTexture texture
            SDL.copy renderer texture Nothing Nothing
            present renderer

    let loop s = do
            before <- ticks
            events <- pollEvents
            keys <- getKeyboardState
            let windowClosed = any isQuitEvent events
            endState <- if windowClosed then return Nothing else runFrame events keys s
            forM_ endState $ \(draw, s') -> do
                render draw
                after <- ticks
                let elapsed = after - before
                when (elapsed < frameTime) $ threadDelay (fromIntegral (frameTime - elapsed) * 1000)
                loop s'
    loop s0

    destroyWindow window
  where
    frameTime = 1000 `div` screenRefreshRate

    isQuitEvent ev = case eventPayload ev of
        WindowClosedEvent{} -> True
        KeyboardEvent KeyboardEventData{ keyboardEventKeysym = Keysym{..}, ..} ->
            keyboardEventKeyMotion == Pressed && keysymKeycode == KeycodeEscape
        _ -> False
