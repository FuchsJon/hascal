module Noekeon where
import Data.Word
import Data.Bits

type Shift     = (Int,Int,Int,Int)
type Noekeon64 = (Word16,Word16,Word16,Word16)
type Slices64  = (Word16,Word16,Word16,Word16)

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
