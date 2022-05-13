{-# LANGUAGE TypeApplications #-}

import Types.Shape
import Types.Heap
import RandomHeap
import Test.QuickCheck
import GHC.IO.Unsafe

toHeapPure :: Shape -> (Shape, Heap)
toHeapPure shape = (shape, unsafePerformIO (toHeap shape))

main :: IO ()
main = do
    sample (toHeapPure <$> arbitrary @Shape)
