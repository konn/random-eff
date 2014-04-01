{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses                      #-}
{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables               #-}
{-# LANGUAGE StandaloneDeriving, TypeOperators                            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Eff.Random (Rand, getRandom, getRandomR, getRandoms, getRandomRs
                          ,getSplit, runRand, evalRand, fromList, uniform
                          ,Generator, evalRandIO, module System.Random) where
import Control.Eff
import Control.Eff.Lift (Lift, lift)
import Control.Monad    (liftM)
import Data.Typeable    (Typeable (..), mkTyCon3, mkTyConApp)
import System.Random

data Generator = forall g. RandomGen g => Generator g

instance Typeable Generator where
  typeOf _ = mkTyConApp gen []
    where
      gen = mkTyCon3 "random-eff" "Control.Eff.Random" "Generator"

data Rand a = Rand (Generator -> (a, Generator))
            deriving (Typeable, Functor)

getRandom :: forall a r. (Typeable a, Random a, Member Rand r) => Eff r a
getRandom = send $ \k ->
  inj $ Rand (\(Generator g) -> let (a :: a, g') = random g
                    in (k a, Generator g'))

getRandoms :: (Random a, Typeable a, Member Rand r) => Eff r [a]
getRandoms = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g
                    in (k (randoms g'), Generator g''))

getRandomR :: (Typeable a, Random a, Member Rand r) => (a, a) -> Eff r a
getRandomR bd = send $ \k ->
  inj $ Rand (\(Generator g) -> let (a, g') = randomR bd g
                    in (k a, Generator g'))

getRandomRs :: (Typeable a, Random a, Member Rand r) => (a, a) -> Eff r [a]
getRandomRs bd = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g in (k (randomRs bd g'), Generator g''))

getSplit :: (Member Rand r) => Eff r Generator
getSplit = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g in (k (Generator g''), Generator g'))

runRand :: RandomGen g => g -> Eff (Rand :> r) w -> Eff r (w, Generator)
runRand g0 = loop (Generator g0) . admin
  where
    loop g (Val x) = return (x, g)
    loop g (E u)   = handleRelay u (loop g) $ \(Rand f) ->
      let (a, g') = f g
      in loop g' a

evalRand :: RandomGen g => g -> Eff (Rand :> r) w -> Eff r w
evalRand g = liftM fst . runRand g

evalRandIO :: SetMember Lift (Lift IO) r => Eff (Rand :> r) w -> Eff r w
evalRandIO eff = do
  g <- lift newStdGen
  evalRand g eff

fromList :: Member Rand r => [(a, Rational)] -> Eff r a
fromList [] = error "Eff.Random.fromList called with empty list"
fromList [(x, _)] = return x
fromList xs = do
  let s  = fromRational $ sum $ map snd xs :: Double
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs
  p <- liftM toRational (getRandomR (0.0,s))
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

uniform :: Member Rand r => [a] -> Eff r a
uniform xs = fromList $ map (flip (,) 1) xs

