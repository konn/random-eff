module Main where
import           Control.Eff
import           Control.Eff.Lift     (runLift)
import           Control.Eff.Random
import qualified Control.Monad.Random as R

main :: IO ()
main = do
  print $ take 100 $ run $ evalRand (mkStdGen 12121) $ getRandomRs (10, 100 :: Int)
  print $ take 100 $ R.evalRand (R.getRandomRs (10, 100 :: Int)) (mkStdGen 12121)
