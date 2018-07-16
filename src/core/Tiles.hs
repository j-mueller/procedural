{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes     #-}
module Tiles where

import           Control.Comonad
import           Data.Foldable        (Foldable (..))
import           Data.Functor.Compose
import           Data.Semigroup

data Tile f a = Tile {
  context :: f (Tile f a),
  content :: a
  } deriving (Functor)

instance (Functor f, Foldable f) => Foldable (Tile f) where
  foldMap f (Tile ct c) = f c <> fold (fmap (foldMap f) ct)

instance Functor f => Comonad (Tile f) where
  extract = content
  extend = instantiate

data TwoDimensional a = TwoDimensional {
    left   :: a,
    right  :: a,
    top    :: a,
    bottom :: a
  } deriving (Functor, Show, Foldable)

instantiate :: Functor f => (Tile f a -> b) -> Tile f a -> Tile f b
instantiate f (Tile ff c) = result where
  result = Tile ff' (f $ Tile ff c)
  ff' = fmap (instantiate f) ff

type TwoDTile a = Tile (Compose TwoDimensional Maybe) (Int, a)

threeByThree :: TwoDTile Int
threeByThree = fmap (\i -> (i, i)) topLeft where
  topLeft = Tile
    (Compose $ TwoDimensional Nothing (Just topMiddle) Nothing (Just middleLeft)) 1
  topMiddle = Tile (Compose $ TwoDimensional (Just topLeft) (Just topRight) Nothing (Just middleMiddle)) 2
  topRight = Tile (Compose $ TwoDimensional (Just topMiddle) Nothing Nothing (Just middleRight)) 3

  middleLeft = Tile (Compose $ TwoDimensional Nothing (Just middleMiddle) (Just topLeft) (Just bottomLeft)) 4
  middleMiddle = Tile (Compose $ TwoDimensional (Just middleLeft) (Just middleRight) (Just topMiddle) (Just bottomMiddle)) 5
  -- middleMiddle = middleLeft
  middleRight = Tile (Compose $ TwoDimensional (Just middleMiddle) Nothing (Just topRight) (Just bottomRight)) 6

  bottomLeft = Tile (Compose $ TwoDimensional Nothing (Just bottomMiddle) (Just middleLeft) Nothing) 7
  bottomMiddle = Tile (Compose $ TwoDimensional (Just bottomLeft) (Just bottomRight) (Just middleMiddle) Nothing) 8
  bottomRight = Tile (Compose $ TwoDimensional (Just bottomMiddle) Nothing (Just middleRight) Nothing) 9

sm :: Foldable f => Tile f Double -> Double
sm (Tile ct _) = getSum $ foldMap (Sum . content) ct
