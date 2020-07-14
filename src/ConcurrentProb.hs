{- |
Copyright: (c) 2020 Armando Santos
SPDX-License-Identifier: MIT
Maintainer: Armando Santos <armandoifsantos@gmail.com>

See README for more info
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module ConcurrentProb where

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Selective
import Data.Bifunctor
import Data.Bool
import Data.List (group, sort)
import Data.Foldable (toList, traverse_)
import Data.IORef
import qualified Data.Vector as V
import Data.Sequence (Seq, singleton)
import GHC.Generics
import qualified System.Random.MWC.Probability as MWCP

data BlockedRequest = forall a. BlockedRequest (Request a) (IORef (Status a))

data Status a = NotFetched | Fetched a

type Prob = Double

data Request a where
  Uniform     :: [x] -> (x -> a) -> Request a
  Categorical :: [(x, Prob)] -> (x -> a) -> Request a
  Normal      :: Double -> Double -> (Double -> a) -> Request a
  Beta        :: Double -> Double -> (Double -> a) -> Request a
  Gamma       :: Double -> Double -> (Double -> a) -> Request a

instance Show a => Show (Request a) where
  show (Uniform l f)     = "Uniform " ++ show (map f l)
  show (Categorical l f) = "Categorical " ++ show (map (first f) l)
  show (Normal x y _)    = "Normal " ++ show x ++ " " ++ show y
  show (Beta x y _)      = "Beta " ++ show x ++ " " ++ show y
  show (Gamma x y _)     = "Gamma " ++ show x ++ " " ++ show y

-- A Haxl computation is either completed (Done) or Blocked on pending data requests
data Result a = Done a | Blocked (Seq BlockedRequest) (Fetch a) deriving Functor

newtype Fetch a = Fetch {unFetch :: IO (Result a)} deriving Functor

instance Applicative Fetch where
  pure = return

  Fetch iof <*> Fetch iox = Fetch $ do
    rf <- iof
    rx <- iox
    return $ case (rf, rx) of
      (Done f, _)                  -> f <$> rx
      (_, Done x)                  -> ($x) <$> rf
      (Blocked bf f, Blocked bx x) -> Blocked (bf <> bx) (f <*> x) -- parallelism

instance Selective Fetch where
  select (Fetch iox) (Fetch iof) = Fetch $ do
    rx <- iox
    rf <- iof
    return $ case (rx, rf) of
      (Done (Right b), _)          -> Done b -- abandon the second computation
      (Done (Left a), _)           -> ($a) <$> rf
      (_, Done f)                  -> either f id <$> rx
      (Blocked bx x, Blocked bf f) -> Blocked (bx <> bf) (select x f) -- speculative execution

instance Monad Fetch where
  return = Fetch . return . Done

  Fetch iox >>= f = Fetch $ do
    rx <- iox
    case rx of
      Done x       -> unFetch (f x) -- dynamic dependency on runtime value 'x'
      Blocked bx x -> return (Blocked bx (x >>= f))

requestSample :: Request a -> Fetch a
requestSample request = Fetch $ do
  box <- newIORef NotFetched
  let br   = BlockedRequest request box
      cont = Fetch $ do
        Fetched a <- readIORef box
        return (Done a)
  return (Blocked (singleton br) cont)

fetch :: [BlockedRequest] -> IO ()
fetch = mapConcurrently_ aux
  where
    aux (BlockedRequest r ref) = do
        threadDelay 100
        c <- MWCP.createSystemRandom
        case r of
          Uniform l f -> do
            i <- MWCP.sample (MWCP.uniformR (0, length l - 1)) c
            writeIORef ref (Fetched . f $ l !! i)
          Categorical l f -> do
            i <- MWCP.sample (MWCP.categorical (V.fromList . map snd $ l)) c
            writeIORef ref (Fetched . f . fst $ l !! i)
          Normal x y f -> do
            a <- MWCP.sample (MWCP.normal x y) c
            writeIORef ref (Fetched . f $ a)
          Beta x y f -> do
            a <- MWCP.sample (MWCP.beta x y) c
            writeIORef ref (Fetched . f $ a)
          Gamma x y f -> do
            a <- MWCP.sample (MWCP.gamma x y) c
            writeIORef ref (Fetched . f $ a)

fetch2 :: [BlockedRequest] -> IO ()
fetch2 = traverse_ aux
  where
    aux (BlockedRequest r ref) = do
        threadDelay 100
        c <- MWCP.createSystemRandom
        case r of
          Uniform l f -> do
            i <- MWCP.sample (MWCP.uniformR (0, length l - 1)) c
            writeIORef ref (Fetched . f $ l !! i)
          Categorical l f -> do
            i <- MWCP.sample (MWCP.categorical (V.fromList . map snd $ l)) c
            writeIORef ref (Fetched . f . fst $ l !! i)
          Normal x y f -> do
            a <- MWCP.sample (MWCP.normal x y) c
            writeIORef ref (Fetched . f $ a)
          Beta x y f -> do
            a <- MWCP.sample (MWCP.beta x y) c
            writeIORef ref (Fetched . f $ a)
          Gamma x y f -> do
            a <- MWCP.sample (MWCP.gamma x y) c
            writeIORef ref (Fetched . f $ a)

runFetch :: Fetch a -> IO a
runFetch (Fetch h) = do
  r <- h
  case r of
    Done a -> return a
    Blocked br cont -> do
      fetch (toList br)
      runFetch cont

runFetch2 :: Fetch a -> IO a
runFetch2 (Fetch h) = do
  r <- h
  case r of
    Done a -> return a
    Blocked br cont -> do
      fetch2 (toList br)
      runFetch2 cont

-- Probabilistic eDSL

uniform :: [a] -> Fetch a
uniform = requestSample . flip Uniform id

categorical :: [(a, Double)] -> Fetch a
categorical = requestSample . flip Categorical id

normal :: Double -> Double -> Fetch Double
normal x y = requestSample (Normal x y id)

bernoulli :: Double -> Fetch Bool
bernoulli x = categorical [(True, x), (False, 1 - x)]

binomial :: Int -> Double -> Fetch Int
binomial n p = length . filter id <$> sequenceA (replicate n (bernoulli p))

beta :: Double -> Double -> Fetch Double
beta x y = requestSample (Beta x y id)

gamma :: Double -> Double -> Fetch Double
gamma x y = requestSample (Gamma x y id)

condition :: (a -> Bool) -> Fetch a -> Fetch (Maybe a)
condition c = condS (pure c) (pure (const Nothing)) (pure Just)

-- Examples of Probabilistic Programs

ex1a :: Fetch (Bool, Bool)
ex1a =
  let c1 = bernoulli 0.5
      c2 = bernoulli 0.5
   in (,) <$> c1 <*> c2

ex1b :: Fetch (Maybe (Bool, Bool))
ex1b =
  let c1 = bernoulli 0.5
      c2 = bernoulli 0.5
      result = (,) <$> c1 <*> c2
   in condition (uncurry (||)) result

ex2 :: Fetch Int
ex2 =
  let count = pure 0
      c1 = bernoulli 0.5
      c2 = bernoulli 0.5
      cond = condition (uncurry (||)) ((,) <$> c1 <*> c2)
      count2 = ifS (maybe False fst <$> cond) count ((+ 1) <$> count)
      count3 = ifS (maybe False snd <$> cond) count2 ((+ 1) <$> count2)
   in count3

ex3 :: Fetch Int
ex3 =
  let count = pure 0
      c1 = bernoulli 0.5
      c2 = bernoulli 0.5
      cond = not . uncurry (||) <$> ((,) <$> c1 <*> c2)
      count2 = ifS c1 count ((+ 1) <$> count)
      count3 = ifS c2 count2 ((+ 1) <$> count2)
   in ifS cond count3 ((+) <$> count3 <*> ex3)

ex4 :: Fetch Bool
ex4 =
  let b = pure True
      c = bernoulli 0.5
   in ifS (not <$> c) b (not <$> ex4)

data Coin = Heads | Tails
  deriving (Show, Eq, Ord, Bounded, Enum, NFData, Generic)

-- Throw 2 coins
t2c :: Fetch (Coin, Coin)
t2c =
  let c1 = bool Heads Tails <$> bernoulli 0.5
      c2 = bool Heads Tails <$> bernoulli 0.5
   in (,) <$> c1 <*> c2

-- Throw 2 coins with condition
t2c2 :: Fetch (Maybe (Bool, Bool))
t2c2 =
  let c1 = bernoulli 0.5
      c2 = bernoulli 0.5
   in condition (uncurry (||)) ((,) <$> c1 <*> c2)

-- | Throw coins until 'Heads' comes up
prog :: Fetch [Coin]
prog =
  let toss = bernoulli 0.5
   in condS
        (pure (== Heads))
        (flip (:) <$> prog)
        (pure (: []))
        (bool Heads Tails <$> toss)

-- | bad toss
throw :: Int -> Fetch [Bool]
throw 0 = pure []
throw n =
  let toss = bernoulli 0.5
   in ifS
        toss
        ((:) <$> toss <*> throw (n - 1))
        (pure [])

-- | This models a simple board game where, at each turn,
-- two dice are thrown and, if the value of the two dice is equal,
-- the face of the third dice is equal to the other dice,
-- otherwise the third die is thrown and one piece moves
-- the number of squares equal to the sum of all the dice.
diceThrow :: Fetch Int
diceThrow =
  condS
    (pure $ uncurry (==))
    ((\c (a, b) -> a + b + c) <$> die) -- Speculative dice throw
    (pure (\(a, _) -> a + a + a))
    ((,) <$> die <*> die) -- Parallel dice throw

diceThrow2 :: Fetch [Int]
diceThrow2 =
  condS
    (pure $ uncurry (==))
    ((\c (a, b) -> [a, b, c]) <$> die) -- Speculative dice throw
    (pure (\(a, b) -> [a, b]))
    ((,) <$> die <*> die) -- Parallel dice throw

die :: Fetch Int
die = uniform [1 .. 6]

-- | Infering the weight of a coin.
--
-- The coin is fair with probability 0.8 and biased with probability 0.2.
weight :: Fetch Prob
weight =
  ifS
    (bernoulli 0.8)
    (pure 0.5)
    (beta 5 1)

-- Sampling/Inference Algorithms

sample :: Fetch a -> Int -> Fetch [a]
sample r n = sequenceA (replicate n r)

-- monte carlo sampling/inference
monteCarlo :: Ord a => Int -> Fetch a -> Fetch [(a, Double)]
monteCarlo n d =
  let r = sample d n
   in map (\l -> (head l, fromIntegral (length l) / fromIntegral n)) . group . sort <$> r

-- Inefficient rejection sampling
rejection :: (Bounded c, Enum c, Eq c) => ([a] -> [b] -> Bool) -> [b] -> Fetch c -> (c -> Fetch a) -> Fetch c
rejection predicate observed proposal model = loop
  where
    len = length observed
    loop =
      let parameters = proposal
          generated = sample (bindS parameters model) len
          cond = predicate <$> generated <*> pure observed
       in ifS
            cond
            parameters
            loop

-- Guard function used in McCarthy's conditional

-- | It provides information about the outcome of testing @p@ on some input @a@,
-- encoded in terms of the coproduct injections without losing the input
-- @a@ itself.
grdS :: Applicative f => f (a -> Bool) -> f a -> f (Either a a)
grdS f a = selector <$> applyF f (dup <$> a)
  where
    dup x = (x, x)
    applyF fab faa = bimap <$> fab <*> pure id <*> faa
    selector (b, x) = bool (Left x) (Right x) b

-- | McCarthy's conditional, denoted p -> f,g is a well-known functional
-- combinator, which suggests that, to reason about conditionals, one may
-- seek help in the algebra of coproducts.
--
-- This combinator is very similar to the very nature of the 'select'
-- operator and benefits from a series of properties and laws.
condS :: Selective f => f (b -> Bool) -> f (b -> c) -> f (b -> c) -> f b -> f c
condS p f g = (\r -> branch r f g) . grdS p

