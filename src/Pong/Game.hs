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
    { wallSize, ballSize :: Int
    , paddleHeight, paddleWidth :: Int
    , paddleSpeed, nudgeSpeed :: Int
    }

defaultParams :: Params
defaultParams = MkParams
    { wallSize = 2
    , ballSize = 5
    , paddleHeight = 50
    , paddleWidth = 5
    , paddleSpeed = 3
    , nudgeSpeed = 3
    }

data Inputs = MkInputs
    { paddleUp :: Bool
    , paddleDown :: Bool
    }

updateState :: Params -> Inputs -> St -> St
updateState params inp = execState $ do
    updateBall params inp
    updatePaddle params inp
    checkBounds params

updateBall :: Params -> Inputs -> State St ()
updateBall params@MkParams{..} MkInputs{..} = do
    updateVert params
    hitPaddle <- updateHoriz params
    when hitPaddle $ ballV._2 += nudge
  where
    nudge | paddleDown = nudgeSpeed
          | paddleUp = negate nudgeSpeed
          | otherwise = 0

updatePaddle :: Params -> Inputs -> State St ()
updatePaddle MkParams{..} MkInputs{..} = do
    when paddleUp $ paddleY -= paddleSpeed
    when paddleDown $ paddleY += paddleSpeed
    paddleY %= clamp (wallSize, screenHeight - (wallSize + paddleHeight))

checkBounds :: Params -> State St ()
checkBounds MkParams{..} = do
    outOfBounds <- zoom ballH $ gets $ \(x, _) -> x > screenWidth
    gameOver .= outOfBounds
    when outOfBounds $ resetBall
  where
    resetBall = ballH._1 .= screenWidth `shiftR` 1

updateHoriz :: Params -> State St Bool
updateHoriz MkParams{..} = do
    atPaddle <- do
        paddleY <- use paddleY
        (y, _) <- use ballV
        return $ y `between` (paddleY - ballSize, paddleY + paddleHeight)
    zoom ballH $ do
        moveM
        reflectM (wallSize, 1)
        if not atPaddle then return False
          else reflectM (screenWidth - paddleWidth - ballSize, -1)

updateVert :: Params -> State St ()
updateVert MkParams{..} = void $ do
    zoom ballV $ do
      moveM
      reflectM (wallSize, 1)
      reflectM (screenHeight - wallSize - ballSize, -1)

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

reflectM :: (Num a, Num a', Ord a, Ord a') => (a, a') -> State (a, a') Bool
reflectM = state . reflect

moveM :: (Num a) => State (a, a) ()
moveM = modify move

x `between` (lo, hi) = lo <= x && x <= hi
