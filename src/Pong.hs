{-# LANGUAGE NumericUnderscores, RecordWildCards, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Pong where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Clock
import Data.Maybe

import Pong.Game
import Pong.Video

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "BTN_UP"    ::: Signal Dom25 (Active High)
    -> "BTN_DOWN"  ::: Signal Dom25 (Active High)
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board (fmap fromActive -> up) (fmap fromActive -> down) = vgaOut vgaSync rgb
      where
        VGADriver{..} = vgaDriver vga640x480at60
        frameEnd = isFalling False (isJust <$> vgaY)

        params = defaultParams
        inputs = MkInputs <$> up <*> down

        st = regEn initState frameEnd $ (updateState params <$> inputs <*> st)

        rgb = fmap (maybe (0, 0, 0) bitCoerce) $
            liftA2 <$> (draw params <$> st) <*> x <*> y
          where
            x = scale @ScreenWidth (SNat @2) . center $ vgaX
            y = scale @ScreenHeight (SNat @2) . center $ vgaY
makeTopEntity 'topEntity
