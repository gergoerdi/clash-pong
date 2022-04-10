{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}
{-# LANGUAGE TypeApplications, LambdaCase #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
module RetroClash.I2C (i2cMaster) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock
import Control.Monad.State
import Data.Maybe (isJust, isNothing)

type Message = (Unsigned 7, Unsigned 8, Unsigned 8)

data MessageState
    = Init Init
    | SendAddr (SendBits 7)
    | SendSubaddr (SendBits 8)
    | SendDat (SendBits 8)
    | Teardown Teardown
    deriving (Show, Generic, BitPack, NFDataX)

data SendBits n
  = SendBit SendTransition (Index n)
  | SendAck SendTransition
  deriving (Show, Generic, NFDataX)
deriving instance (KnownNat n, 1 <= n) => BitPack (SendBits n)

data SendTransition = SDASet | Tick | Tock
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

data Init = StartInit | SDALow | SCLLow
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

data Teardown = StartTeardown | SCLHigh | SDAHigh
  deriving (Show, Enum, Bounded, Eq, Generic, BitPack, NFDataX)

startBit :: (KnownNat n) => SendBits n
startBit = SendBit minBound maxBound

nextBit :: (KnownNat n) => SendBits n -> Maybe (SendBits n)
nextBit (SendBit transition i) = Just $ case succIdx transition of
    Just transition' -> SendBit transition' i
    Nothing -> maybe (SendAck minBound) (SendBit minBound) $ predIdx i
nextBit (SendAck transition) = SendAck <$> succIdx transition

shiftOut :: (KnownNat n) => Unsigned n -> SendBits n -> (Maybe Bit, Maybe Bit)
shiftOut x (SendBit transition i) = (Just $ x ! i, Just $ boolToBit $ transition == Tick)
shiftOut _ (SendAck transition) = (Nothing, Just $ boolToBit $ transition == Tick)

-- We only drive clk (clock stretching not implemented), and we never query
-- peripherals over I2C, so we never actually use sdaIn and sclIn
i2cNext :: Bool -> Bit -> Bit -> Maybe MessageState -> Maybe MessageState
i2cNext startNew _sdaIn _sclIn = \case
    Nothing              -> Init StartInit <$ guard startNew

    Just (Init ramp)     -> Just $ maybe (SendAddr startBit) Init $ succIdx ramp

    Just (SendAddr b)    -> Just $ maybe (SendSubaddr startBit) SendAddr $ nextBit b
    Just (SendSubaddr b) -> Just $ maybe (SendDat startBit) SendSubaddr $ nextBit b
    Just (SendDat b)     -> Just $ maybe (Teardown StartTeardown) SendDat $ nextBit b

    Just (Teardown ramp) -> Teardown <$> succIdx ramp

i2cOutput :: Message -> Maybe MessageState -> (Maybe Bit, Maybe Bit)
i2cOutput (addr, subaddr, dat) = \case
    Nothing -> (Just 1, Just 1)

    Just (Init StartInit)         -> (Just 1, Just 1)
    Just (Init SDALow)            -> (Just 0, Just 1)
    Just (Init SCLLow)            -> (Just 0, Just 0)

    Just (SendAddr b)             -> shiftOut addr b
    Just (SendSubaddr b)          -> shiftOut subaddr b
    Just (SendDat b)              -> shiftOut dat b

    Just (Teardown StartTeardown) -> (Just 0, Just 0)
    Just (Teardown SCLHigh)       -> (Just 0, Just 1)
    Just (Teardown SDAHigh)       -> (Just 1, Just 1)

i2cMaster
    :: (HiddenClockResetEnable dom, 1 <= i2cRate, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => SNat i2cRate
    -> Signal dom (Maybe Message)
    -> Signal dom Bit
    -> Signal dom Bit
    -> (Signal dom Bool, Signal dom (Maybe Bit, Maybe Bit))
i2cMaster i2cRate@SNat msg sdaIn sclIn = mealyStateB step Nothing (i2cClock, msg, sdaIn, sclIn)
  where
    i2cClock = riseRate i2cRate

    step :: (Bool, Maybe Message, Bit, Bit) -> State (Maybe MessageState) (Bool, (Maybe Bit, Maybe Bit))
    step (tick, msg, sdaIn, sclIn) = do
        s <- get
        when tick $ modify $ i2cNext (isJust msg) sdaIn sclIn
        s' <- get
        return (tick && isNothing s', i2cOutput (fromJustX msg) s)
