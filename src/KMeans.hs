module KMeans where

import Control.DeepSeq

import Data.Binary
import Data.Foldable (minimumBy)
import Data.Function

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV

import GHC.Generics

-- -----------------------------------------------------------------------------
-- Point

data Point =
  Point {-#UNPACK#-} !Double
        {-#UNPACK#-} !Double
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Generic
    )

instance NFData Point

instance Binary Point where
  put (Point x y) = put x >> put y
  get = do x <- get; Point x <$> get

sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1 - x2)^2) + ((y1 - y2)^2)

readPoints :: FilePath -> IO [Point]
readPoints f = do
  s <- B.readFile f
  let ls = map B.words $ B.lines s
      points = [ Point (read (B.unpack xs)) (read (B.unpack ys)) | (xs:ys:_) <- ls ]
  return points

-- -----------------------------------------------------------------------------
-- PointSum

data PointSum = PointSum !Int !Double !Double
  deriving (Eq, Show)

instance Semigroup PointSum where
  (PointSum c1 x1 y1) <> (PointSum c2 x2 y2) = PointSum (c1+c2) (x1+x2) (y1+y2)

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (Point x y) =
  PointSum (count + 1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster id (PointSum count xs ys) =
  Cluster
    { clId = id
    , clCent = Point (xs / fromIntegral count) (ys / fromIntegral count)
    }

combinePointSums :: V.Vector PointSum -> V.Vector PointSum -> V.Vector PointSum
combinePointSums = V.zipWith (<>)

-- -----------------------------------------------------------------------------
-- Cluster

data Cluster = Cluster
  { clId   :: {-# UNPACK #-} !Int
  , clCent :: {-# UNPACK #-} !Point
  }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Generic
    )

instance NFData Cluster

makeNewClusters :: V.Vector PointSum -> [Cluster]
makeNewClusters vec =
  [ pointSumToCluster cid ps
  | (cid, ps@(PointSum count _ _)) <- zip [0..] (V.toList vec)
  , count > 0
  ]

-- -----------------------------------------------------------------------------
-- Assign points to clusters

assign :: Int -> [Cluster] -> [Point] -> V.Vector PointSum
assign n clusters points = V.create $ do
  vec <- MV.replicate n (PointSum 0 0 0)
  let
    addpoint p = do
      let c = nearest p; cid = clId c
      ps <- MV.read vec cid
      MV.write vec cid $! addToPointSum ps p
  mapM_ addpoint points
  return vec
  where
    distances p = [(c, sqDistance (clCent c) p) | c <- clusters]
    nearest p = fst $ minimumBy (compare `on` snd) (distances p)

