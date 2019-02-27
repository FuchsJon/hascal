import Data.Word
import Control.Monad
import Control.Parallel.Strategies
import Prelude hiding (all)

type Predicate a = ([a] -> Bool)

isEven :: (Num a,Integral a,Foldable t) => t a -> Bool
isEven = even . foldr (+) 0

all :: (Bounded a, Enum a) => [a]
all = [minBound .. maxBound]

appendLayer :: (Bounded a,Enum a,Num a) => Predicate a -> [a] -> [[a]]
appendLayer f input = do
                         nextLayer <- [0..10]
                         guard  $f $ nextLayer : input
                         return $    nextLayer : input

parAppendLayer :: (Bounded a,Enum a,Num a,NFData a) => Predicate a -> [a] -> [[a]]
parAppendLayer f input = let bs = appendLayer f input
                             cs = bs `using` parList rdeepseq
                         in cs

generateLayers :: (Bounded a,Enum a,Num a) => Int -> Predicate a -> [[a]]
generateLayers n p = concat . take n $ iterate (>>= appendLayer p) (appendLayer p [])

parGenerateLayers :: (Bounded a,Enum a,Num a,NFData a) => Int -> Predicate a -> [[a]]
parGenerateLayers n p = concat . take n $ iterate (>>= parAppendLayer p) (parAppendLayer p [])

