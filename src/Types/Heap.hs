{-# OPTIONS_GHC -O1 #-}
{-# OPTIONS_GHC -ddump-to-file -ddump-stg-final #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Types.Heap
    ( Heap(..)
    , mkLifted, mkUnlifted
    , retLifted, retUnlifted
    ) where

import GHC.Exts (Any, TYPE, RuntimeRep(..), Levity(..), unsafeCoerce#)
import qualified GHC.Exts.Heap as H
import GHC.IO.Unsafe
import Test.QuickCheck

data Heap where
    Heap :: Any -> Heap

instance Show Heap where
    show (Heap x) = show (unsafePerformIO $ H.getClosureData x)

mkLifted :: forall a. a -> Heap
mkLifted x = let x' = unsafeCoerce# x in Heap x'

mkUnlifted :: forall (a :: TYPE ('BoxedRep 'Unlifted)). a -> Heap
mkUnlifted x# = let x' = unsafeCoerce# x# in Heap x'

retLifted
    :: forall a.  a -> IO Heap
retLifted x = return (mkLifted x)

retUnlifted
    :: forall (a :: TYPE ('BoxedRep 'Unlifted)). a -> IO Heap
retUnlifted x = return (mkUnlifted x)

