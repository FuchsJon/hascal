module MultiNoekeon where
import Data.Word
import Data.Bits
import Text.Printf
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

defaultRotate = rotateState (0,1,5,2)

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
-Predicates
-}
fill :: (Num a ) => Int -> [a] -> [a]
fill n ls = take n $ ls ++ repeat 0

validWeight n input =let s  = fill 4 input
                         s' = slicesToState s
                         w  = wa s' + wb s' 
                     in n >= w && w > 0  
