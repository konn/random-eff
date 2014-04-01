{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts            #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction       #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TypeOperators #-}
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
import System.Random

-- | Wrapper Type for 'RandomGen' types
--
-- Since 0.1.0.0
data Generator = forall g. RandomGen g => Generator g
#if MIN_VERSION_base(4,7,0)
                 deriving (Typeable)
#else
instance Typeable Generator where
  typeOf _ = mkTyConApp gen []
    where
      gen = mkTyCon3 "random-eff" "Control.Eff.Random" "Generator"
#endif

-- | This behaves exactly as same as the original, un-quantified instance.
--
-- Since 0.1.0.0
instance RandomGen Generator where
  next (Generator g) = second Generator $ next g
  genRange (Generator g) = genRange g
  split (Generator g) =
    let (h, h') = split g
    in (Generator h, Generator h')

-- | Random number generator
--
-- Since 0.1.0.0
data Rand a = Rand (Generator -> (a, Generator))
              deriving (Typeable)

instance Functor Rand where
  fmap g (Rand f) = Rand (first g . f)

-- | Return a randomly-selected value of type a. See 'random' for details.
--
-- Since 0.1.0.0
getRandom :: forall a r. (Typeable a, Random a, Member Rand r) => Eff r a
getRandom = send $ \k ->
  inj $ Rand (\(Generator g) -> let (a :: a, g') = random g
                    in (k a, Generator g'))

-- | Return an infinite stream of random values of type a. See 'randoms' for details.
--
-- Since 0.1.0.0
getRandoms :: (Random a, Typeable a, Member Rand r) => Eff r [a]
getRandoms = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g
                    in (k (randoms g'), Generator g''))

-- | Return a randomly-selected value of type a in the range @(lo,hi)@. See 'randomR' for details.
--
-- Since 0.1.0.0
getRandomR :: (Typeable a, Random a, Member Rand r) => (a, a) -> Eff r a
getRandomR bd = send $ \k ->
  inj $ Rand (\(Generator g) -> let (a, g') = randomR bd g
                    in (k a, Generator g'))

-- | Return an infinite stream of randomly-selected value of type a in the range @(lo,hi)@. See 'randomRs' for details.
--
-- Since 0.1.0.0
getRandomRs :: (Typeable a, Random a, Member Rand r) => (a, a) -> Eff r [a]
getRandomRs bd = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g in (k (randomRs bd g'), Generator g''))

-- | Sample a random value from a weighted list. The total weight of all elements must not be 0.
--
-- Since 0.1.0.0
fromList :: Member Rand r => [(a, Rational)] -> Eff r a
fromList [] = error "Eff.Random.fromList called with empty list"
fromList [(x, _)] = return x
fromList xs = do
  let s  = fromRational $ sum $ map snd xs :: Double
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs
  p <- liftM toRational (getRandomR (0.0,s))
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

-- | Sample a value from a uniform distribution of a list of elements.
--
-- Since 0.1.0.0
uniform :: Member Rand r => [a] -> Eff r a
uniform xs = fromList $ map (flip (,) 1) xs

-- | Split the internal generator. This returns the second result of 'split'
--   and set the new internal generator to the first one.
--
-- Since 0.1.0.0
getSplit :: (Member Rand r) => Eff r Generator
getSplit = send $ \k ->
  inj $ Rand (\(Generator g) -> let (g', g'') = split g in (k (Generator g''), Generator g'))

-- | Run a computation with random numbers
--
-- Since 0.1.0.0
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
--
-- Since 0.1.0.0
evalRand :: RandomGen g => g -> Eff (Rand :> r) w -> Eff r w
evalRand g = liftM fst . runRand g

-- | Run a computation with random numbers, using 'newStdGen' as its initial generator.
--
-- Since 0.1.0.0
evalRandIO :: SetMember Lift (Lift IO) r => Eff (Rand :> r) w -> Eff r w
evalRandIO eff = do
  g <- lift newStdGen
  evalRand g eff
