{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

module RandomHeap
    ( Heap(..)
    , fun, funAp, funPap
    , dynamicConApp, staticConApp
    , mvar
    ) where

import Test.QuickCheck
import qualified GHC.Exts.Heap as H
import Control.Monad.IO.Class
import GHC.IO.Unsafe
import GHC.Conc (TVar(..), newTVarIO)
import GHC.Exts (Any, TYPE, RuntimeRep(..), Levity(..), unsafeCoerce#)
import Control.Monad (replicateM)

import qualified Data.Primitive.Array as A
import qualified Data.Primitive.SmallArray as SA
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Primitive.MVar as MVar

data Heap where
    Heap :: Any -> Heap

instance Show Heap where
    show (Heap x) = show (unsafePerformIO $ H.getClosureData x)

mkLifted :: forall a. a -> Heap
mkLifted x = let !x = unsafeCoerce# x in Heap x

mkUnlifted :: forall (a :: TYPE ('BoxedRep 'Unlifted)). a -> Heap
mkUnlifted x# = let !x = unsafeCoerce# x# in Heap x

fun2 :: Integer -> Integer -> Integer
fun2 a b = a + b
{-# NOINLINE fun2 #-}

staticCon :: Maybe Int
staticCon = Just 42
{-# NOINLINE staticCon #-}

instance Arbitrary Heap where
    arbitrary = oneof
        [ fun
        , funAp
        , funPap
        , selectorApp
        , nullaryCon
        , dynamicConApp
        , staticConApp
        , mvar
        , arrPtrs
        , smallArrPtrs
        , arrWords
        , tvar
        -- ioref
        ]

fun :: Gen Heap
fun = return $ mkLifted id

nullaryCon :: Gen Heap
nullaryCon = return $ mkLifted (Nothing @Int)

dynamicConApp :: Gen Heap
dynamicConApp = do
    Heap x <- arbitrary
    return $ mkLifted (Just x)

funAp :: Gen Heap
funAp = do
    Heap x <- arbitrary
    return $ mkLifted (id x)

funPap :: Gen Heap
funPap = do
    !x <- arbitrary :: Gen Integer
    return $ mkLifted $ fun2 x

selectorApp :: Gen Heap
selectorApp = do
    x <- arbitrary :: Gen (Int, Int)
    return $ mkLifted x

staticConApp :: Gen Heap
staticConApp = return (mkLifted staticCon)

mvar :: Gen Heap
mvar = oneof
    [ do let !(MVar.MVar mvar) = unsafePerformIO $ MVar.newEmptyMVar @IO @Int
         return (mkUnlifted mvar)
    , do let !(MVar.MVar mvar) = unsafePerformIO $ MVar.newMVar @IO @Int 42
         return (mkUnlifted mvar)
    ]

arrPtrs :: Gen Heap
arrPtrs = do
    n <- arbitrary
    values <- replicateM n arbitrary
    let !(A.MutableArray arr#) = unsafePerformIO $ do
            arr <- A.newArray n undefined
            let writeOne i (Heap x) = A.writeArray arr i x
            sequence_ $ zipWith writeOne [0..n-1] values
            return arr
    return (mkUnlifted arr#)

smallArrPtrs :: Gen Heap
smallArrPtrs = do
    n <- arbitrary
    values <- replicateM n arbitrary
    let !(SA.SmallMutableArray arr#) = unsafePerformIO $ do
            arr <- SA.newSmallArray n undefined
            let writeOne i (Heap x) = SA.writeSmallArray arr i x
            sequence_ $ zipWith writeOne [0..n-1] values
            return arr
    return (mkUnlifted arr#)

arrWords :: Gen Heap
arrWords = do
    n <- arbitrary
    let !(BA.MutableByteArray arr#) = unsafePerformIO $ BA.newByteArray n
    return (mkUnlifted arr#)

tvar :: Gen Heap
tvar = do
    let !(TVar tvar#) = unsafePerformIO $ newTVarIO (42 :: Int)
    return (mkUnlifted tvar#)
