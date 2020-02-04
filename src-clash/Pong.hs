{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo, ViewPatterns, TupleSections #-}
module Pong where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Clock
import Data.Maybe

import Pong.Game

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "Pong"
    , t_inputs =
          [ PortName "CLK_25MHZ"
          , PortName "RESET"
          , PortName "SWITCHES"
          , PortName "BTN_UP"
          , PortName "BTN_DOWN"
          ]
    , t_output =
            vgaPort
    }) #-}
topEntity
    :: Clock Dom25
    -> Reset Dom25
    -> Signal Dom25 (Vec 8 Bit)
    -> Signal Dom25 (Active High)
    -> Signal Dom25 (Active High)
    -> VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board switches (fmap fromActive -> up) (fmap fromActive -> down) = vgaOut vgaSync rgb
      where
        VGADriver{..} = vgaDriver vga640x480at60
        frameEnd = isFalling False (isJust <$> vgaY)

        params = pure defaultParams
        inputs = MkInputState <$> up <*> down

        st = regEn initState frameEnd $ (updateState <$> params <*> inputs <*> st)

        rgb = bitCoerce <$> (draw <$> params <*> st <*> xy)
          where
            xy = fromMaybe (0, 0) <$> (liftA2 (,) <$> vgaX <*> vgaY)
