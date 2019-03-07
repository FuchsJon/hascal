module MultiNoekeon where
import Data.Word
import Data.Bits
import Text.Printf
import Data.List
import qualified Data.Vector as Vec

type Noekeon a = (a,a,a,a)
type Slice     =  Word16
type Slices    = [Word16]

{------------------------------------------------------------------------------------
 - In this section you can find all the functions that are using to convert between
 - the slice representation and the normal noekeon representation.
 ------------------------------------------------------------------------------------}

mask :: Word16
mask =  0x1111

-- Extracts a single row from a slice
getRow :: Int -> Slice -> Word16
getRow i input =  (((mask `shiftL` i) .&. input)`shiftR`i)

--Given a slice and its z value returns the correct Noekeon state
shiftSlice :: Int -> Slice -> Noekeon Word16
shiftSlice i input = let (a0,a1,a2,a3) = sliceToState input
                     in  (a0 `shiftL` i,a1 `shiftL` i,a2 `shiftL` i,a3 `shiftL` i)

-- This funciton is used to combine Noekeon states created from different slices into a single Noekeon state.
fuseState :: Noekeon Word16 -> Noekeon Word16 -> Noekeon Word16
fuseState (a,b,c,d) (a',b',c',d') = (a .|. a', b .|. b', c .|. c', d .|. d')

-- Takes a single slice and returns a Noekeon state
sliceToState :: Slice -> Noekeon Word16
sliceToState slice = (getRow 0 slice,getRow 1 slice,getRow 2 slice,getRow 3 slice)


-- Takes a list of slices and turns it into a Noekeon state

slicesToState :: Slices -> Noekeon Word16
slicesToState ls =  foldr1 fuseState $Vec.imap shiftSlice (Vec.fromList ls)

--Prints the binary representation of a Noekeon State, mainly used for debugging.
printState noekeonvariant (a0,a1,a2,a3) =   let printmode = "%b\n"
                                            in  printf (printmode++printmode++printmode++printmode) a0 a1 a2 a3


{---------------------------------------------------------------------------
----------------------------------------------------------------------------
--The functions used during a single round of Noekeon are implemented below.
----------------------------------------------------------------------------
----------------------------------------------------------------------------}

theta :: Noekeon Word16 -> Noekeon Word16
theta (a,b,c,d) = let temp1 = a `xor` c
                      temp2 = temp1 `xor` (temp1 `rotateL` 4)
                      temp3 = temp2 `xor` (temp1 `rotateR` 4)
                      b'    = b `xor` temp3
                      d'    = d `xor` temp3
                      temp4 = b' `xor` d'
                      temp5 = temp4 `xor` (temp4 `rotateL` 4)
                      temp6 = temp5 `xor` (temp4 `rotateR` 4)
                      a'    = a `xor` temp6
                      c'    = c `xor` temp6
                   in (a',b',c',d')

rotateState :: (Int,Int,Int,Int) -> Noekeon Word16 -> Noekeon Word16
rotateState (r1,r2,r3,r4) (a,b,c,d) = (a,b `rotateR` r2,c `rotateR` r3,d `rotateR` r4)

rotateState' :: (Int,Int,Int,Int) -> Noekeon Word16 -> Noekeon Word16
rotateState' (r1,r2,r3,r4) (a,b,c,d) = (a,b `rotateL` r2,c `rotateL` r3,d `rotateL` r4)

defaultRotate = rotateState (0,1,5,2)
defaultRotate' = rotateState' (0,1,5,2)

cipher = defaultRotate.theta.defaultRotate'

afterGamma  = defaultRotate
beforeGamma = defaultRotate.theta
{-
-Weight related functions
-}

stateWeight :: Noekeon Word16 -> Int
stateWeight (a,b,c,d) = popCount(a .|. b .|. c .|. d) 

wa = stateWeight.afterGamma
wb = stateWeight.beforeGamma

{-
-Symmetry group functions
-}
shiftSymGroup :: Slices -> [Slices]
shiftSymGroup ls = take 4 $ iterate (fmap ((flip rotateL) 4)) ls

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

sliceSymGroup :: Int -> Slices -> [Slices]
sliceSymGroup i ls = take i $ iterate (rotateList 1) ls

symGroup :: Int -> Slices -> [Slices]
symGroup i ls = concatMap (sliceSymGroup i) (shiftSymGroup ls)

isZmin :: Int -> Slices -> Bool
isZmin i filled = let 
                      sym    = symGroup i filled
                      zmin   = maximum sym
                  in  filled == zmin

{-
Performance
-}

{-
-Predicates
-}
fill :: (Num a ) => Int -> [a] -> [a]
fill n ls = take n $ ls ++ repeat 0

validWeight n filled =let
                         s' = slicesToState filled
                         w  = wa s' + wb s' 
                      in n >= w && w > 0  

isValid slices = let filled = fill 4 slices
                 in (validWeight 8 filled) && (isZmin 4 filled)  

{--
Cols
--}



getBit     :: (Bits a) => Int -> a-> a
getBit i n = let b = bit i .&. n
             in  b `shiftR` i

getCol :: (Bits a) =>  Noekeon a -> Int  -> a
getCol (a0,a1,a2,a3) i = let    a = getBit i a0
                                b = getBit i a1 `shiftL`1
                                c = getBit i a2 `shiftL`2
                                d = getBit i a3 `shiftL`3
                         in  a.|.b.|.c.|.d 
getCols :: (Bits a) =>Noekeon a -> [a]
getCols n = fmap (getCol n) [0..15]

getStateRow  :: [Word16] -> Int -> Word16
getStateRow n i =  foldr1 (.|.) $Vec.imap (flip shiftL) (Vec.fromList $fmap (getBit i) n)
getStateRows :: [Word16] -> Noekeon Word16
getStateRows n = (getStateRow n 0,getStateRow n 1,getStateRow n 2, getStateRow n 3)

{-
-- trail props
-}

getProps :: Word16 -> [Word16]
getProps 0   = [0]
getProps 1   = [0xC,0xD,0xE,0xF]
getProps 2   = [4,8,5,6,7,0xB]
getProps 4   = [2,4,8,3,6,0xA,0xC,0xD]
getProps 8   = [2,4,3,5,6,7]
getProps 3   = [4,8,5,6,7,0xB]
getProps 5   = [2,8,3,5,0xA,7,0xE,0xF]
getProps 6   = [2,4,8,3,6,0xA,0xE,0xF]
getProps 9   = [9,0xC,0xB,0xD,0xE,0xF]
getProps 0xA = [4,5,6,0xC,7,0xD,0xE,0xF]
getProps 0xC = [1,4,9,0xA,7,0xE]
getProps 7   = [2,8,3,5,0xA,0xC,7,0xD]
getProps 0xB = [2,3,9,0xB]
getProps 0xD = [1,4,9,0xA,7,0xD]
getProps 0xE = [1,5,6,9,0xA,0xC]
getProps 0xF = [1,5,6,9,0xA,0xF]

extPredic :: Int -> Noekeon Word16 -> Bool
extPredic i n = stateWeight n < i

extendRound :: Int ->Noekeon Word16 -> [Noekeon Word16]
extendRound i n = filter (extPredic i) $fmap (cipher.getStateRows) $ mapM getProps (getCols n)
 
