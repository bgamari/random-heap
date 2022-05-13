{-# LANGUAGE TypeApplications #-}

import RandomHeap
import Test.QuickCheck

main :: IO ()
main = do
    sample (arbitrary @Heap)
