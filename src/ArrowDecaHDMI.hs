{-# LANGUAGE TypeApplications, FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
module ArrowDecaHDMI (topEntity) where

import Clash.Prelude
import Clash.Annotations.TH
import Data.Bifunctor
import RetroClash.Utils
import RetroClash.I2C

createDomain vSystem{vName="Dom50", vPeriod = hzToPeriod 50_000_000}

type I2CFreq = 20_000

topEntity
    :: "CLK_50MHZ" ::: Clock Dom50
    -> "RESET"     ::: Reset Dom50
    -> "I2C_SCL" ::: BiSignalIn 'PullUp Dom50 (BitSize Bit)
    -> "I2C_SDA" ::: BiSignalIn 'PullUp Dom50 (BitSize Bit)
    -> "I2C" :::
       ( "SCL" ::: BiSignalOut 'PullUp Dom50 (BitSize Bit)
       , "SDA" ::: BiSignalOut 'PullUp Dom50 (BitSize Bit)
       )
topEntity clk reset = exposeClockResetEnable (run (SNat @I2CFreq)) clk reset enableGen

run
    :: (HiddenClockResetEnable dom, 1 <= i2cRate, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => SNat i2cRate
    -> "SCL_IN"  ::: BiSignalIn 'PullUp dom (BitSize Bit)
    -> "SDA_IN"  ::: BiSignalIn 'PullUp dom (BitSize Bit)
    -> ( "SCL_OUT" ::: BiSignalOut 'PullUp dom (BitSize Bit)
       , "SDA_OUT" ::: BiSignalOut 'PullUp dom (BitSize Bit)
       )
run i2cRate sclIn sdaIn = (sclOut, sdaOut)
  where
    (sclOut, sdaOut, ready) = i2cMaster i2cRate msg sclIn sdaIn
    msg = fmap (\(subAddr, x) -> (0x72, pack subAddr, pack x)) . (initHDMI !!.) <$> i
    i = regEn (Just (0 :: Index DatLength)) ready $ (succIdx =<<) <$> i

-- Data to be output over I2C
type DatLength = 31

initHDMI :: Vec DatLength (Unsigned 8, Unsigned 8)
initHDMI = $(listToVecTH @(Unsigned 8, Unsigned 8)
  [(0x98, 0x03) -- Must be set to 0x03 for proper operation
  ,(0x01, 0x00) -- Set 'N' value at 6144
  ,(0x02, 0x18) -- Set 'N' value at 6144
  ,(0x03, 0x00) -- Set 'N' value at 6144
  ,(0x14, 0x70) -- Set Ch count in the channel status to 8.
  ,(0x15, 0x20) -- Input 444 (RGB or YCrCb) with Separate Syncs, 48kHz fs
  ,(0x16, 0x30) -- Output format 444, 24-bit input
  ,(0x18, 0x46) -- Disable CSC
  ,(0x40, 0x80) -- General control packet enable
  ,(0x41, 0x10) -- Power down control
  ,(0x49, 0xA8) -- Set dither mode - 12-to-10 bit
  ,(0x55, 0x10) -- Set RGB in AVI infoframe
  ,(0x56, 0x08) -- Set active format aspect
  ,(0x96, 0xF6) -- Set interrup
  ,(0x73, 0x07) -- Info frame Ch count to 8
  ,(0x76, 0x1f) -- Set speaker allocation for 8 channels
  ,(0x98, 0x03) -- Must be set to 0x03 for proper operation
  ,(0x99, 0x02) -- Must be set to Default Value
  ,(0x9a, 0xe0) -- Must be set to 0b1110000
  ,(0x9c, 0x30) -- PLL filter R1 value
  ,(0x9d, 0x61) -- Set clock divide
  ,(0xa2, 0xa4) -- Must be set to 0xA4 for proper operation
  ,(0xa3, 0xa4) -- Must be set to 0xA4 for proper operation
  ,(0xa5, 0x04) -- Must be set to Default Value
  ,(0xab, 0x40) -- Must be set to Default Value
  ,(0xaf, 0x16) -- Select HDMI mode
  ,(0xba, 0x60) -- No clock delay
  ,(0xd1, 0xff) -- Must be set to Default Value
  ,(0xde, 0x10) -- Must be set to Default for proper operation
  ,(0xe4, 0x60) -- Must be set to Default Value
  ,(0xfa, 0x7d) -- Nbr of times to look for good phase
  ])

makeTopEntityWithName 'topEntity "initHDMI"
