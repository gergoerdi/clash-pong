{-# LANGUAGE RecordWildCards, TemplateHaskell, RankNTypes, TypeApplications #-}
module Pong.Game
    ( Params(..)
    , defaultParams

    , Inputs(..)
    , St(..)
    , initState
    , updateState

    , ScreenWidth
    , ScreenHeight

    , between
    ) where

import Prelude
import Clash.Prelude hiding (lift)
import RetroClash.Utils

import Data.Word
import Control.Monad.State
import Control.Lens hiding (Index)

move :: (Num a) => (a, a) -> (a, a)
move (x, dx) = (x + dx, dx)

clamp :: (Ord a) => (a, a) -> a -> a
clamp (lo, hi) = max lo . min hi

reflect :: (Num a, Num a', Ord a, Ord a') => (a, a') -> (a, a') -> (Bool, (a, a'))
reflect (p, n) (x, dx)
    | sameDirection n diff = (True, (p + diff, negate dx))
    | otherwise = (False, (x, dx))
  where
    sameDirection u v = compare 0 u == compare 0 v
    diff = p - x

type ScreenWidth = 256
type ScreenHeight = 200

screenWidth :: Int
screenWidth = snatToNum (SNat @ScreenWidth)

screenHeight :: Int
screenHeight = snatToNum (SNat @ScreenHeight)

data St = MkSt
    { _ballH, _ballV :: (Int, Int)
    , _paddleY :: Int
    , _gameOver :: Bool
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''St

initState :: St
initState = MkSt
    { _ballH = (10, 2)
    , _ballV = (100, 3)
    , _paddleY = 100
    , _gameOver = False
    }

data Params = MkParams
    { wallSize, ballSize, paddleSize :: Int
    , paddleWidth, paddleSpeed, nudgeSpeed :: Int
    }

data Inputs = MkInputs
    { paddleUp :: Bool
    , paddleDown :: Bool
    }

reflectM :: (Num a, Num a', Ord a, Ord a') => (a, a') -> State (a, a') Bool
reflectM = state . reflect

moveM :: (Num a) => State (a, a) ()
moveM = modify move

x `between` (lo, hi) = lo <= x && x <= hi

updateHoriz :: Params -> State St Bool
updateHoriz MkParams{..} = do
    atPaddle <- do
        paddleY <- use paddleY
        (y, _) <- use ballV
        return $ y `between` (paddleY, paddleY + paddleSize)
    zoom ballH $ do
        moveM
        reflectM (wallSize, 1)
        if not atPaddle then return False else reflectM (screenWidth - paddleWidth - ballSize, -1)

updateVert :: Params -> State St ()
updateVert MkParams{..} = void $ do
    zoom ballV $ do
      moveM
      reflectM (wallSize, 1)
      reflectM (screenHeight - wallSize - ballSize, -1)

updatePaddle :: Params -> Inputs -> State St ()
updatePaddle MkParams{..} MkInputs{..} = do
    when paddleUp $ paddleY -= paddleSpeed
    when paddleDown $ paddleY += paddleSpeed
    paddleY %= clamp (wallSize, screenHeight - (wallSize + paddleSize))

updateState :: Params -> Inputs -> St -> St
updateState params@MkParams{..} inp@MkInputs{..} = execState $ do
    gameOver .= False

    updateVert params
    hitPaddle <- updateHoriz params
    when hitPaddle $ ballV._2 += nudge

    updatePaddle params inp
    outOfBounds <- zoom ballH $ gets $ \(x, _) -> x > screenWidth
    when outOfBounds $ do
        gameOver .= True
        ballH._1 .= screenWidth `shiftR` 1
  where
    nudge | paddleDown = nudgeSpeed
          | paddleUp = negate nudgeSpeed
          | otherwise = 0

defaultParams :: Params
defaultParams = MkParams
    { wallSize = 2
    , ballSize = 5
    , paddleSize = 50
    , paddleWidth = 5
    , paddleSpeed = 3
    , nudgeSpeed = 3
    }
