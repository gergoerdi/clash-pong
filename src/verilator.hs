{-# LANGUAGE RecordWildCards, OverloadedStrings, NumericUnderscores #-}
module Main where

import Prelude
import Clash.Prelude (boolToBit)

import RetroClash.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL

import SimInterface
import VerilatorFFI
import Foreign.Storable
import Foreign.Marshal.Alloc

import SDL hiding (get)
import Control.Monad
import Control.Monad.State
import Data.Array.IO
import Control.Lens

import Data.Int
import System.Clock
import Text.Printf
import Control.Monad.Loops
import Data.Text

millisec :: TimeSpec -> Int64
millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> do
    sim <- simInit
    let step input = do
            poke inp input
            simStep sim inp outp
            peek outp
    x <- act step
    simShutdown sim
    return x

main :: IO ()
main = withRunner $ \runCycle -> do
    buf <- newBufferArray
    t0 <- getTime Monotonic

    flip evalStateT (initSink, (0, t0)) $ withMainWindow "Pong" 2 $ \events keyState -> fmap Just $ do
        let input = INPUT
                { reset = False
                , btnUp = boolToBit $ keyState ScancodeUp
                , btnDown = boolToBit $ keyState ScancodeDown
                }

        untilM_ (return ()) $ do
            vgaOut <- do
                OUTPUT{..} <- liftIO $ runCycle input
                return (vgaHSYNC, vgaVSYNC, (vgaRED, vgaGREEN, vgaBLUE))
            zoom _1 $ vgaSinkBuf vga640x480at60 buf vgaOut

        zoom _2 $ do
            (i, t0) <- get
            if i == 60 then do
                t <- liftIO $ getTime Monotonic
                let dt = millisec t - millisec t0
                    fps = 1000 / (fromIntegral dt / 60) :: Double
                liftIO $ printf "60 frames in %d ms, %.1f fps\n" dt fps
                put (0, t)
              else put (i + 1, t0)

        return $ rasterizeBuffer buf
