{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -ddump-to-file -ddump-stg-final #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

module RandomHeap
    ( toHeap
    ) where

import Types.Heap
import Types.Shape

import Test.QuickCheck
import Control.Monad.IO.Class
import GHC.IO.Unsafe
import GHC.Conc (TVar(..), newTVarIO)
import GHC.Exts (Any, TYPE, RuntimeRep(..), Levity(..), unsafeCoerce#)
import Control.Monad (replicateM)

import qualified Data.Primitive.Array as A
import qualified Data.Primitive.SmallArray as SA
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Primitive.MVar as MVar

fun2 :: Integer -> Integer -> Integer
fun2 a b = a + b
{-# NOINLINE fun2 #-}

staticCon :: Maybe Int
staticCon = Just 42
{-# NOINLINE staticCon #-}

toHeap :: Shape -> IO Heap
toHeap (UnaryConApp x) = retLifted (Just x)
toHeap NullaryConApp = retLifted (Nothing @Int)
toHeap StaticUnaryConApp = retLifted staticCon
toHeap CAF = retLifted caf
toHeap (Thunk (SomeObj obj)) = retLifted (obj + 2)
toHeap (SelectorApp x) = retLifted (fst x)
toHeap FUN = retLifted id
toHeap (AP x) = retLifted (id x)
toHeap (PAP x) = retLifted (id x) -- TODO
toHeap (MVAR Nothing) = do
    MVar.MVar mvar <- MVar.newEmptyMVar @IO @Int
    retUnlifted mvar
toHeap (MVAR (Just x)) = do
    MVar.MVar mvar <- MVar.newMVar x
    retUnlifted mvar
toHeap (TVAR x) = do
    TVar tvar <- newTVarIO x
    retUnlifted tvar
toHeap (ArrPtrs xs) = do
    let n = length xs
    arr <- A.newArray n undefined
    sequence_ $ zipWith (A.writeArray arr) [0..] xs
    A.MutableArray arr# <- pure arr
    return (mkUnlifted arr#)
toHeap (SmallArrPtrs xs) = do
    let n = length xs
    arr <- SA.newSmallArray n undefined
    sequence_ $ zipWith (SA.writeSmallArray arr) [0..] xs
    SA.SmallMutableArray arr# <- pure arr
    return (mkUnlifted arr#)
toHeap (ArrWords n) = do
    BA.MutableByteArray arr# <- BA.newByteArray n
    return (mkUnlifted arr#)

caf :: Int
caf = sum [1,2,3,4,5]

