{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses                      #-}
{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables               #-}
{-# LANGUAGE StandaloneDeriving, TypeOperators                            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Eff.Random (module System.Random,
                           Rand, Generator(..),
                           -- * Execution
                           runRand, evalRand, evalRandIO,
                           -- * Generator functions
                           getRandom, getRandomR, getRandoms, getRandomRs,
                           fromList, uniform,
                           -- * Misc
                           getSplit) where
import Control.Arrow    (first)
import Control.Arrow    (second)
import Control.Eff
import Control.Eff.Lift (Lift, lift)
import Control.Monad    (liftM)
import Data.Typeable    (Typeable (..), mkTyCon3, mkTyConApp)
import Data.Typeable    (Typeable1 (..), typeOfDefault)
import System.Random

-- | Wrapper Type for 'RandomGen' types
data Generator = forall g. RandomGen g => Generator g

instance Typeable Generator where
  typeOf _ = mkTyConApp gen []
    where
      gen = mkTyCon3 "random-eff" "Control.Eff.Random" "Generator"

-- | This behaves exactly as same as the original, un-quantified instance.
instance RandomGen Generator where
  next (Generator g) = second Generator $ next g
  genRange (Generator g) = genRange g
  split (Generator g) =
    let (h, h') = split g
    in (Generator h, Generator h')

-- | Random number generator
data Rand a = Rand (Generator -> (a, Generator))

instance Functor Rand where
  fmap g (Rand f) = Rand (first g . f)

instance Typeable1 Rand where
  typeOf1 _ = mkTyConApp (mkTyCon3 "package" "Control.Eff.Random" "Rand") []

instance Typeable a => Typeable (Rand a) where
  typeOf = typeOfDefault

-- | Return a randomly-selected value of type a. See 'random' for details.
getRandom :: forall a r. (Typeable a, Random a, Member Rand r) => Eff r a
getRandom = send $ \k ->
  inj $ Rand (\(Generator g) -> let (a :: a, g') = random g
                    in (k a, Generator g'))

-- | Return an infinite stream of random values of type a. See 'randoms' for details.
getRandoms :: (Random a, Typeable a, Member Rand r) => Eff r [a]
getRandoms = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g
                    in (k (randoms g'), Generator g''))

-- | Return a randomly-selected value of type a in the range @(lo,hi)@. See 'randomR' for details.
getRandomR :: (Typeable a, Random a, Member Rand r) => (a, a) -> Eff r a
getRandomR bd = send $ \k ->
  inj $ Rand (\(Generator g) -> let (a, g') = randomR bd g
                    in (k a, Generator g'))

-- | Return an infinite stream of randomly-selected value of type a in the range @(lo,hi)@. See 'randomRs' for details.
getRandomRs :: (Typeable a, Random a, Member Rand r) => (a, a) -> Eff r [a]
getRandomRs bd = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g in (k (randomRs bd g'), Generator g''))


-- | Sample a random value from a weighted list. The total weight of all elements must not be 0.
fromList :: Member Rand r => [(a, Rational)] -> Eff r a
fromList [] = error "Eff.Random.fromList called with empty list"
fromList [(x, _)] = return x
fromList xs = do
  let s  = fromRational $ sum $ map snd xs :: Double
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs
  p <- liftM toRational (getRandomR (0.0,s))
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

-- | Sample a value from a uniform distribution of a list of elements.
uniform :: Member Rand r => [a] -> Eff r a
uniform xs = fromList $ map (flip (,) 1) xs

-- | Split the internal generator. This returns the second result of 'split'
--   and set the new internal generator to the first one.
getSplit :: (Member Rand r) => Eff r Generator
getSplit = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g in (k (Generator g''), Generator g'))

-- | Run a computation with random numbers
runRand :: RandomGen g
        => g                    -- ^ initial internal random generator
        -> Eff (Rand :> r) w    -- ^ Effect using random numbers
        -> Eff r (w, Generator)
        -- ^ Effect containing return value and final random number generator.
        -- The generator is returned as existential type due to the limitation
        -- of the current implementation, but it's guaranteed to work exactly
        -- as same as the original given generator type.
runRand g0 = loop (Generator g0) . admin
  where
    loop g (Val x) = return (x, g)
    loop g (E u)   = handleRelay u (loop g) $ \(Rand f) ->
      let (a, g') = f g
      in loop g' a

-- | Run a computation with random numbers, discarding the final generator.
evalRand :: RandomGen g => g -> Eff (Rand :> r) w -> Eff r w
evalRand g = liftM fst . runRand g

-- | Run a computation with random numbers, using 'newStdGen' as its initial generator.
evalRandIO :: SetMember Lift (Lift IO) r => Eff (Rand :> r) w -> Eff r w
evalRandIO eff = do
  g <- lift newStdGen
  evalRand g eff
