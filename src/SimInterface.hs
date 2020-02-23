module SimInterface where

import Prelude
import Clash.Prelude

import Data.Word
import Data.Int

data INPUT = INPUT
    { reset :: Bool
    , switches :: Word8
    , btnUp :: Bit
    , btnDown :: Bit
    }
    deriving (Show)

data OUTPUT = OUTPUT
    { vgaHSYNC, vgaVSYNC :: Bit
    , vgaDE :: Bool
    , vgaRED, vgaGREEN, vgaBLUE :: Word8
    }
    deriving (Show)
