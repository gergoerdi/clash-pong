{-# LANGUAGE NumericUnderscores, RecordWildCards, TypeApplications #-}
{-# LANGUAGE ApplicativeDo, ViewPatterns, TupleSections #-}
module Pong where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Clock
import Data.Maybe

import Pong.Game
import Pong.Video

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "Pong"
    , t_inputs =
          [ PortName "CLK_25MHZ"
          , PortName "RESET"
          , PortName "BTN_UP"
          , PortName "BTN_DOWN"
          ]
    , t_output =
            vgaPort
    }) #-}
topEntity
    :: Clock Dom25
    -> Reset Dom25
    -> Signal Dom25 (Active High)
    -> Signal Dom25 (Active High)
    -> VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board (fmap fromActive -> up) (fmap fromActive -> down) = vgaOut vgaSync rgb
      where
        VGADriver{..} = vgaDriver vga640x480at60
        frameEnd = isFalling False (isJust <$> vgaY)

        params = pure defaultParams
        inputs = MkInputState <$> up <*> down

        st = regEn initState frameEnd $ (updateState <$> params <*> inputs <*> st)

        rgb = fmap (maybe (0, 0, 0) bitCoerce) $ do
            x <- scale (SNat @2) . center @(2 * ScreenWidth) $ vgaX
            y <- scale (SNat @2) . center @(2 * ScreenHeight) $ vgaY
            params <- params
            st <- st
            pure $ draw params st <$> ((,) <$> x <*> y)
