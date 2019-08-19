import Test.QuickCheck
import Data.Word
import Data.Bits
import MultiNoekeon


translationInverse :: Noekeon Word16 -> Noekeon Word16
translationInverse =  getStateRows.getCols

checkPredicate :: Noekeon Word16 -> Bool
checkPredicate input = translationInverse input == input


main = quickCheck (withMaxSuccess 65535 checkPredicate ) 
