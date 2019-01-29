import Test.QuickCheck
import Noekeon
thetaInverse :: Noekeon64 -> Noekeon64
thetaInverse = cipher . cipher

checkInverse :: Noekeon64 -> Bool
checkInverse input = thetaInverse input == input

main = quickCheck (withMaxSuccess 65535 checkInverse ) 
