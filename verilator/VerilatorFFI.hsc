{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}
module VerilatorFFI where

import Prelude
import Clash.Prelude

import SimInterface

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

#include "VerilatorAPI.h"

data Sim

foreign import ccall unsafe "vinit" simInit :: IO (Ptr Sim)
foreign import ccall unsafe "vshutdown" simShutdown :: Ptr Sim -> IO ()
foreign import ccall unsafe "vstep" simStep :: Ptr Sim -> Ptr INPUT -> Ptr OUTPUT -> IO ()

instance Storable Bit where
    alignment = alignment . bitToBool
    sizeOf = sizeOf . bitToBool
    peek = fmap boolToBit . peek . castPtr
    poke ptr = poke (castPtr ptr) . bitToBool

instance Storable INPUT where
    alignment _ = #alignment INPUT
    sizeOf _ = #size INPUT
    {-# INLINE peek #-}
    peek ptr    = INPUT
        <$> (#peek INPUT, RESET) ptr
        <*> (#peek INPUT, BTN_UP) ptr
        <*> (#peek INPUT, BTN_DOWN) ptr
    {-# INLINE poke #-}
    poke ptr INPUT{..} = do
        (#poke INPUT, RESET) ptr reset
        (#poke INPUT, BTN_UP) ptr btnUp
        (#poke INPUT, BTN_DOWN) ptr btnDown

instance Storable OUTPUT where
    alignment _ = #alignment OUTPUT
    sizeOf _ = #size OUTPUT
    {-# INLINE peek #-}
    peek ptr    = OUTPUT
        <$> (#peek OUTPUT, VGA_HSYNC) ptr
        <*> (#peek OUTPUT, VGA_VSYNC) ptr
        <*> (#peek OUTPUT, VGA_DE)    ptr
        <*> (#peek OUTPUT, VGA_RED)   ptr
        <*> (#peek OUTPUT, VGA_GREEN) ptr
        <*> (#peek OUTPUT, VGA_BLUE)  ptr
    {-# INLINE poke #-}
    poke ptr OUTPUT{..} = do
        (#poke OUTPUT, VGA_HSYNC) ptr vgaHSYNC
        (#poke OUTPUT, VGA_VSYNC) ptr vgaVSYNC
        (#poke OUTPUT, VGA_DE)    ptr vgaDE
        (#poke OUTPUT, VGA_RED)   ptr vgaRED
        (#poke OUTPUT, VGA_GREEN) ptr vgaGREEN
        (#poke OUTPUT, VGA_BLUE)  ptr vgaBLUE
