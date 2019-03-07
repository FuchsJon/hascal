import Iterator
import MultiNoekeon

round3 = concatMap (extendRound 8)$ fmap slicesToState $ generateLayers 4 isValid
round4 = concatMap (extendRound 5) round3

main :: IO()
main = putStrLn $ show $ round4
