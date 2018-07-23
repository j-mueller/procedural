{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module Tiles where

import           Control.Monad                     (join)
import           Control.Monad.IO.Class
import           Data.Foldable                     (Foldable (..))
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, isNothing,
                                                    listToMaybe)
import           Data.Semigroup
import qualified Data.Sequence                     as Seq
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Square
import           Math.Geometry.Grid.SquareInternal (SquareDirection (..))
import           System.Random.MWC                 (asGenST, uniformR,
                                                    withSystemRandom)

type family Neighbours a :: *

newtype Generator k v = Generator { getGen :: Map k (Maybe v) }

-- | Populate the map using a generator function
runGen :: (Monad m, Ord k)
  => (k -> Neighbours k -> Maybe k)
  -> ((Neighbours k -> Maybe v) -> m v)
  -> Generator k v
  -> m (Map k v)
runGen lkp eval (Generator initial) = go initial where
  go mp = maybe (return $ Map.fromList $ catMaybes $ sequenceA <$> Map.toList mp) proceed lst where
    lst = listToMaybe $ filter (isNothing . snd) $ Map.toList mp
    proceed (k, _) = do
      let f nbh = lkp k nbh >>= join . flip Map.lookup mp
      v <- eval f
      go $ Map.insert k (Just v) mp

-- | Populate the tiles of a finite grid in random order using a generator
--   function
runGridGen :: (MonadIO m, Monad m, Grid g, Ord (Index g), Eq (Direction g), FiniteGrid g, Size g ~ (Int, Int))
  => g
  -> ((Direction g -> Maybe v) -> m v) -- ^ How to generate a tile 'v' from the values of its neighbours
  -> Map (Index g) v -- ^ Tiles that have already been assigned
  -> m (Map (Index g) v) -- ^ A map assigning a value to each tile
runGridGen gr eval = go where
  sz = size gr
  numTiles = uncurry (*) sz
  go mp =
    if Map.size mp >= numTiles
    then return mp
    else fillOneTile gr eval mp >>= go

fillOneTile :: (MonadIO m, Monad m, Grid g, Ord (Index g), Eq (Direction g))
  => g
  -> ((Direction g -> Maybe v) -> m v) -- ^ How to generate a tile 'v' from the values of its neighbours
  -> Map (Index g) v -- ^ Tiles that have already been assigned
  -> m (Map (Index g) v) -- ^ The original map with a new tile added
fillOneTile gr evl initial = result where
  emptyTiles = Seq.fromList
    $ take 10000 -- avoid an infinite loop if the grid is unbounded
    $ filter (not . flip Map.member initial)
    $ indices gr
  result =
    if Seq.null emptyTiles
    then return initial
    else do
      let l = Seq.length emptyTiles
      i <- liftIO $ withSystemRandom . asGenST $ uniformR (0, pred l)
      let Just idx = Seq.lookup i emptyTiles
          f nbh    = neighbour gr idx nbh >>= flip Map.lookup initial
      v <- evl f
      return $ Map.insert idx v initial

run :: IO (Map (Int, Int) Int)
run = runGridGen theGrid evl Map.empty where
  theGrid = rectSquareGrid 3 3
  evl f = result where
    sm = getSum $ foldMap Sum $ catMaybes $ fmap f [North, East, South, West]
    result = if sm > 1 then return sm else return 2
