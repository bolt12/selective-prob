module Main (main) where

import Criterion.Main
import qualified SelectiveProb as S
import qualified ConcurrentProb as C

-- Selective
normalWithoutConditioningS :: Int -> S.Dist [(S.Coin, S.Coin)]
normalWithoutConditioningS = S.sample S.t2c

normalWithConditioningS :: Int -> S.Dist [Maybe (Bool, Bool)]
normalWithConditioningS = S.sample S.t2c2

normalWithSpeculativeExecutionS :: Int -> S.Dist [Int]
normalWithSpeculativeExecutionS = S.sample S.diceThrow

mCWithoutConditioningS :: Int -> S.Dist [((S.Coin, S.Coin), Double)]
mCWithoutConditioningS n = S.monteCarlo n S.t2c

mCWithConditioningS :: Int -> S.Dist [(Maybe (Bool, Bool), Double)]
mCWithConditioningS n = S.monteCarlo n S.t2c2

mCWithSpeculativeExecutionS :: Int -> S.Dist [(Int, Double)]
mCWithSpeculativeExecutionS n = S.monteCarlo n S.diceThrow

-- Concurrent Monad
normalWithoutConditioningC :: Int -> C.Fetch [(C.Coin, C.Coin)]
normalWithoutConditioningC = C.sample C.t2c

normalWithConditioningC :: Int -> C.Fetch [Maybe (Bool, Bool)]
normalWithConditioningC = C.sample C.t2c2

normalWithSpeculativeExecutionC :: Int -> C.Fetch [Int]
normalWithSpeculativeExecutionC = C.sample C.diceThrow

mCWithoutConditioningC :: Int -> C.Fetch [((C.Coin, C.Coin), Double)]
mCWithoutConditioningC n = C.monteCarlo n C.t2c

mCWithConditioningC :: Int -> C.Fetch [(Maybe (Bool, Bool), Double)]
mCWithConditioningC n = C.monteCarlo n C.t2c2

mCWithSpeculativeExecutionC :: Int -> C.Fetch [(Int, Double)]
mCWithSpeculativeExecutionC n = C.monteCarlo n C.diceThrow

main :: IO ()
main = defaultMain [
    bgroup "Selective Sequential S" [
      bgroup "1000x" [
        bench "Norm" $ nfIO (S.runToIO (normalWithoutConditioningS 1000)),
        bench "Cond" $ nfIO (S.runToIO (normalWithConditioningS 1000)),
        bench "Spec" $ nfIO (S.runToIO (normalWithSpeculativeExecutionS 1000))
                           ],
      bgroup "5000x" [
        bench "Norm" $ nfIO (S.runToIO (normalWithoutConditioningS 5000)),
        bench "Cond" $ nfIO (S.runToIO (normalWithConditioningS 5000)),
        bench "Spec" $ nfIO (S.runToIO (normalWithSpeculativeExecutionS 5000))
                            ],
      bgroup "10000x" [
        bench "Norm" $ nfIO (S.runToIO (normalWithoutConditioningS 10000)),
        bench "Cond" $ nfIO (S.runToIO (normalWithConditioningS 10000)),
        bench "Spec" $ nfIO (S.runToIO (normalWithSpeculativeExecutionS 10000))
                             ],
      bgroup "20000x" [
        bench "Norm" $ nfIO (S.runToIO (normalWithoutConditioningS 20000)),
        bench "Cond" $ nfIO (S.runToIO (normalWithConditioningS 20000)),
        bench "Spec" $ nfIO (S.runToIO (normalWithSpeculativeExecutionS 20000))
                              ]
                             ],
    bgroup "Selective Concurrent S" [
      bgroup "1000x" [
        bench "Norm" $ nfIO (S.runToIO2 (normalWithoutConditioningS 1000)),
        bench "Cond" $ nfIO (S.runToIO2 (normalWithConditioningS 1000)),
        bench "Spec" $ nfIO (S.runToIO2 (normalWithSpeculativeExecutionS 1000))
                           ],
      bgroup "5000x" [
        bench "Norm" $ nfIO (S.runToIO2 (normalWithoutConditioningS 5000)),
        bench "Cond" $ nfIO (S.runToIO2 (normalWithConditioningS 5000)),
        bench "Spec" $ nfIO (S.runToIO2 (normalWithSpeculativeExecutionS 5000))
                            ],
      bgroup "10000x" [
        bench "Norm" $ nfIO (S.runToIO2 (normalWithoutConditioningS 10000)),
        bench "Cond" $ nfIO (S.runToIO2 (normalWithConditioningS 10000)),
        bench "Spec" $ nfIO (S.runToIO2 (normalWithSpeculativeExecutionS 10000))
                             ],
      bgroup "20000x" [
        bench "Norm" $ nfIO (S.runToIO2 (normalWithoutConditioningS 20000)),
        bench "Cond" $ nfIO (S.runToIO2 (normalWithConditioningS 20000)),
        bench "Spec" $ nfIO (S.runToIO2 (normalWithSpeculativeExecutionS 20000))
                              ]
                             ],
    bgroup "Monad Sequential S" [
      bgroup "1000x" [
        bench "Norm" $ nfIO (C.runFetch2 (normalWithoutConditioningC 1000)),
        bench "Cond" $ nfIO (C.runFetch2 (normalWithConditioningC 1000)),
        bench "Spec" $ nfIO (C.runFetch2 (normalWithSpeculativeExecutionC 1000))
                           ],
      bgroup "5000x" [
        bench "Norm" $ nfIO (C.runFetch2 (normalWithoutConditioningC 5000)),
        bench "Cond" $ nfIO (C.runFetch2 (normalWithConditioningC 5000)),
        bench "Spec" $ nfIO (C.runFetch2 (normalWithSpeculativeExecutionC 5000))
                            ],
      bgroup "10000x" [
        bench "Norm" $ nfIO (C.runFetch2 (normalWithoutConditioningC 10000)),
        bench "Cond" $ nfIO (C.runFetch2 (normalWithConditioningC 10000)),
        bench "Spec" $ nfIO (C.runFetch2 (normalWithSpeculativeExecutionC 10000))
                             ],
      bgroup "20000x" [
        bench "Norm" $ nfIO (C.runFetch2 (normalWithoutConditioningC 20000)),
        bench "Cond" $ nfIO (C.runFetch2 (normalWithConditioningC 20000)),
        bench "Spec" $ nfIO (C.runFetch2 (normalWithSpeculativeExecutionC 20000))
                              ]
                             ],
    bgroup "Monad Concurrent S" [
      bgroup "1000x" [
        bench "Norm" $ nfIO (C.runFetch (normalWithoutConditioningC 1000)),
        bench "Cond" $ nfIO (C.runFetch (normalWithConditioningC 1000)),
        bench "Spec" $ nfIO (C.runFetch (normalWithSpeculativeExecutionC 1000))
                           ],
      bgroup "5000x" [
        bench "Norm" $ nfIO (C.runFetch (normalWithoutConditioningC 5000)),
        bench "Cond" $ nfIO (C.runFetch (normalWithConditioningC 5000)),
        bench "Spec" $ nfIO (C.runFetch (normalWithSpeculativeExecutionC 5000))
                            ],
      bgroup "10000x" [
        bench "Norm" $ nfIO (C.runFetch (normalWithoutConditioningC 10000)),
        bench "Cond" $ nfIO (C.runFetch (normalWithConditioningC 10000)),
        bench "Spec" $ nfIO (C.runFetch (normalWithSpeculativeExecutionC 10000))
                             ],
      bgroup "20000x" [
        bench "Norm" $ nfIO (C.runFetch (normalWithoutConditioningC 20000)),
        bench "Cond" $ nfIO (C.runFetch (normalWithConditioningC 20000)),
        bench "Spec" $ nfIO (C.runFetch (normalWithSpeculativeExecutionC 20000))
                              ]
                             ],
    bgroup "Selective Sequential MC" [
      bgroup "1000x" [
        bench "Norm" $ nfIO (S.runToIO (mCWithoutConditioningS 1000)),
        bench "Cond" $ nfIO (S.runToIO (mCWithConditioningS 1000)),
        bench "Spec" $ nfIO (S.runToIO (mCWithSpeculativeExecutionS 1000))
                           ],
      bgroup "5000x" [
        bench "Norm" $ nfIO (S.runToIO (mCWithoutConditioningS 5000)),
        bench "Cond" $ nfIO (S.runToIO (mCWithConditioningS 5000)),
        bench "Spec" $ nfIO (S.runToIO (mCWithSpeculativeExecutionS 5000))
                            ],
      bgroup "10000x" [
        bench "Norm" $ nfIO (S.runToIO (mCWithoutConditioningS 10000)),
        bench "Cond" $ nfIO (S.runToIO (mCWithConditioningS 10000)),
        bench "Spec" $ nfIO (S.runToIO (mCWithSpeculativeExecutionS 10000))
                                            ],
      bgroup "20000x" [
        bench "Norm" $ nfIO (S.runToIO (mCWithoutConditioningS 20000)),
        bench "Cond" $ nfIO (S.runToIO (mCWithConditioningS 20000)),
        bench "Spec" $ nfIO (S.runToIO (mCWithSpeculativeExecutionS 20000))
                                            ]
                         ],
    bgroup "Selective Concurrent MC" [
      bgroup "1000x" [
        bench "Norm" $ nfIO (S.runToIO2 (mCWithoutConditioningS 1000)),
        bench "Cond" $ nfIO (S.runToIO2 (mCWithConditioningS 1000)),
        bench "Spec" $ nfIO (S.runToIO2 (mCWithSpeculativeExecutionS 1000))
                           ],
      bgroup "5000x" [
        bench "Norm" $ nfIO (S.runToIO2 (mCWithoutConditioningS 5000)),
        bench "Cond" $ nfIO (S.runToIO2 (mCWithConditioningS 5000)),
        bench "Spec" $ nfIO (S.runToIO2 (mCWithSpeculativeExecutionS 5000))
                            ],
      bgroup "10000x" [
        bench "Norm" $ nfIO (S.runToIO2 (mCWithoutConditioningS 10000)),
        bench "Cond" $ nfIO (S.runToIO2 (mCWithConditioningS 10000)),
        bench "Spec" $ nfIO (S.runToIO2 (mCWithSpeculativeExecutionS 10000))
                                            ],
      bgroup "20000x" [
        bench "Norm" $ nfIO (S.runToIO2 (mCWithoutConditioningS 20000)),
        bench "Cond" $ nfIO (S.runToIO2 (mCWithConditioningS 20000)),
        bench "Spec" $ nfIO (S.runToIO2 (mCWithSpeculativeExecutionS 20000))
                                            ]
                         ],
    bgroup "Monad Sequential MC" [
      bgroup "1000x" [
        bench "Norm" $ nfIO (C.runFetch2 (mCWithoutConditioningC 1000)),
        bench "Cond" $ nfIO (C.runFetch2 (mCWithConditioningC 1000)),
        bench "Spec" $ nfIO (C.runFetch2 (mCWithSpeculativeExecutionC 1000))
                           ],
      bgroup "5000x" [
        bench "Norm" $ nfIO (C.runFetch2 (mCWithoutConditioningC 5000)),
        bench "Cond" $ nfIO (C.runFetch2 (mCWithConditioningC 5000)),
        bench "Spec" $ nfIO (C.runFetch2 (mCWithSpeculativeExecutionC 5000))
                            ],
      bgroup "10000x" [
        bench "Norm" $ nfIO (C.runFetch2 (mCWithoutConditioningC 10000)),
        bench "Cond" $ nfIO (C.runFetch2 (mCWithConditioningC 10000)),
        bench "Spec" $ nfIO (C.runFetch2 (mCWithSpeculativeExecutionC 10000))
                                            ],
      bgroup "20000x" [
        bench "Norm" $ nfIO (C.runFetch2 (mCWithoutConditioningC 20000)),
        bench "Cond" $ nfIO (C.runFetch2 (mCWithConditioningC 20000)),
        bench "Spec" $ nfIO (C.runFetch2 (mCWithSpeculativeExecutionC 20000))
                                            ]
                         ],
    bgroup "Monad Concurrent MC" [
      bgroup "1000x" [
        bench "Norm" $ nfIO (C.runFetch (mCWithoutConditioningC 1000)),
        bench "Cond" $ nfIO (C.runFetch (mCWithConditioningC 1000)),
        bench "Spec" $ nfIO (C.runFetch (mCWithSpeculativeExecutionC 1000))
                           ],
      bgroup "5000x" [
        bench "Norm" $ nfIO (C.runFetch (mCWithoutConditioningC 5000)),
        bench "Cond" $ nfIO (C.runFetch (mCWithConditioningC 5000)),
        bench "Spec" $ nfIO (C.runFetch (mCWithSpeculativeExecutionC 5000))
                            ],
      bgroup "10000x" [
        bench "Norm" $ nfIO (C.runFetch (mCWithoutConditioningC 10000)),
        bench "Cond" $ nfIO (C.runFetch (mCWithConditioningC 10000)),
        bench "Spec" $ nfIO (C.runFetch (mCWithSpeculativeExecutionC 10000))
                                            ],
      bgroup "20000x" [
        bench "Norm" $ nfIO (C.runFetch (mCWithoutConditioningC 20000)),
        bench "Cond" $ nfIO (C.runFetch (mCWithConditioningC 20000)),
        bench "Spec" $ nfIO (C.runFetch (mCWithSpeculativeExecutionC 20000))
                                            ]
                         ]
                   ]
