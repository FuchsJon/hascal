module Noekeon where
import Data.Word
import Data.Bits
import qualified Data.Vector as Vec

type Shift     = (Int,Int,Int,Int)
type Noekeon64 = (Word16,Word16,Word16,Word16)
type Slices64  = (Word16,Word16,Word16,Word16)
type Slice     =  Word16 


sliceMask :: Word16
sliceMask = 0x1111

deslice :: Int -> Word16 -> Word16
deslice i n = ((n .&.(sliceMask `shiftL` i))`shiftR`i)

sliceToState :: Word16 -> Noekeon64
sliceToState n = let a0 = deslice 0 n
                     a1 = deslice 1 n
                     a2 = deslice 2 n
                     a3 = deslice 3 n
                 in  (a0,a1,a2,a3)

shiftSlice :: Int -> Word16 -> Noekeon64
shiftSlice i n = let (a0,a1,a2,a3) = sliceToState n
                 in  (a0 `rotateL` i,a1 `rotateL` i,a2 `rotateL` i,a3 `rotateL` i)

orNoekeon :: Noekeon64 -> Noekeon64 -> Noekeon64
orNoekeon (a,b,c,d) (e,f,g,h) = ((.|.) a e, (.|.) b f, (.|.) c g, (.|.) d h)

toState :: [Word16] -> Noekeon64
toState ls = foldl orNoekeon (0,0,0,0) $ Vec.imap shiftSlice (Vec.fromList ls)


defaultShift :: Shift
defaultShift = (0,1,5,2)

reverseShift :: Shift -> Shift
reverseShift (s1,s2,s3,s4) = (-s1,-s2,-s3,-s4) :: (Int,Int,Int,Int)

p1 = pis defaultShift
p2 = pis . reverseShift $ defaultShift
pis :: Shift -> Noekeon64 -> Noekeon64
pis (s1,s2,s3,s4) (a0,a1,a2,a3) = let a0' = rotateL a0 s1
                                      a1' = rotateL a1 s2
                                      a2' = rotateL a2 s3
                                      a3' = rotateL a3 s4
                                  in  (a0',a1',a2',a3')

theta :: Noekeon64 -> Noekeon64
theta (a0,a1,a2,a3) = let t  = xor a0 a2
                          t' = xor t  $xor (rotateL t 4) (rotateR t 4)
                          a1'= xor a1 t'
                          a3'= xor a3 t'
                          y  = xor a1' a3'
                          y' = xor y  $xor (rotateL y 4) (rotateR y 4)
                          a0'= xor a0 y'
                          a2'= xor a2 y'
                      in (a0',a1',a2',a3')

cipher :: Noekeon64 -> Noekeon64
cipher = p2 . theta . p1

w2 = p2 . theta
w1 = p2

fuseState :: Noekeon64 -> Word16
fuseState (a,b,c,d) = a .|. b .|. c .|. d

weightOfState :: Noekeon64 -> Int
weightOfState = popCount.fuseState

twoRoundWeight   :: Noekeon64 -> Int
twoRoundWeight n = weightOfState (w1 n) + weightOfState(w2 n)

isWithinBound :: Int -> Noekeon64 -> Bool
isWithinBound w n = w >= w' && w' > 0
                    where w' = twoRoundWeight n 

weightPredicate n = isWithinBound 6 (toState n)

fill :: (Num a ) => Int -> [a] -> [a]
fill n ls = take n $ ls ++ repeat 0

shiftSymGroup :: [Word16] -> [[Word16]]
shiftSymGroup ls = take 4 $ iterate (fmap ((flip rotateL) 4)) ls

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

sliceSymGroup :: Int -> [Word16] -> [[Word16]]
sliceSymGroup i ls = take i $ iterate (rotateList 1) ls

symGroup :: Int -> [Word16] -> [[Word16]]
symGroup i ls = concatMap (sliceSymGroup i) (shiftSymGroup ls)

isZmin :: Int -> [Word16] -> Bool
isZmin i slices = let filled = fill i slices
                      sym    = symGroup i filled
                      zmin   = maximum sym
                  in  filled == zmin

searchPredicate n = weightPredicate n 

getCoord' :: Noekeon64 -> (Int,Int)
getCoord' n = ( weightOfState$ w1 n,weightOfState$ w2 n)

getCoord = getCoord'.toState

tabulate :: [[Word16]] -> [(Int,Int)]
tabulate = fmap getCoord



