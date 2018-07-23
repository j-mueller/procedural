{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
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
import qualified Data.Set                          as Set
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

-- | Populate the tiles of a grid in random order using a generator function
runGridGen :: (MonadIO m, Monad m, Grid g, Ord (Index g), Eq (Direction g))
  => g
  -> ((Direction g -> Maybe v) -> m v)
  -> Map (Index g) v -- ^ Initial constraints (may be empty)
  -> m (Map (Index g) v)
runGridGen gr eval initial = go initialSet initial where
  initialSet = Seq.fromList $ Set.toList $ Set.difference
                (Set.fromList $ indices gr)
                (Set.fromList $ Map.keys initial)
  go st mp =
    if Seq.null st
    then return mp
    else do
      let l = Seq.length st
      i <- liftIO $ withSystemRandom . asGenST $ uniformR (0, pred l)
      let Just idx = Seq.lookup i st
          st'      = Seq.deleteAt i st
          f nbh    = neighbour gr idx nbh >>= flip Map.lookup mp
      v <- eval f
      go st' $ Map.insert idx v mp

run :: IO (Map (Int, Int) Int)
run = runGridGen theGrid evl Map.empty where
  theGrid = rectSquareGrid 3 3
  evl f = result where
    sm = getSum $ foldMap Sum $ catMaybes $ fmap f [North, East, South, West]
    result = if sm > 1 then return sm else return 2
