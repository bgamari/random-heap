{-# OPTIONS_GHC -O1 #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Types.Shape
    ( Shape(..)
    , SomeObj(..)
    ) where

import qualified GHC.Exts as GHC
import qualified GHC.Exts.Heap as H
import Test.QuickCheck

newtype SomeObj = SomeObj Integer
    deriving (Show)

instance Arbitrary SomeObj where
    arbitrary = SomeObj <$> arbitrary

data Shape where
    -- Constructor applications
    UnaryConApp :: SomeObj -> Shape
    NullaryConApp :: Shape  
    StaticUnaryConApp :: Shape

    -- Thunks
    CAF :: Shape
    Thunk :: SomeObj -> Shape
    SelectorApp :: (SomeObj, SomeObj) -> Shape

    -- Functions
    FUN :: Shape
    AP :: SomeObj -> Shape
    PAP :: SomeObj -> Shape

    -- Unlifted things
    MVAR :: Maybe SomeObj -> Shape
    TVAR :: SomeObj -> Shape
    ArrPtrs :: [SomeObj] -> Shape
    SmallArrPtrs :: [SomeObj] -> Shape
    ArrWords :: Int -> Shape
    deriving (Show)

instance Arbitrary Shape where
    arbitrary = oneof
        [ UnaryConApp <$> arbitrary
        , pure NullaryConApp
        , pure StaticUnaryConApp

        , pure CAF
        , Thunk <$> arbitrary
        , SelectorApp <$> arbitrary

        , pure FUN
        , AP <$> arbitrary
        , PAP <$> arbitrary

        , MVAR <$> arbitrary
        , TVAR <$> arbitrary
        , ArrPtrs <$> arbitrary
        , SmallArrPtrs <$> arbitrary
        , ArrWords <$> arraySize
        ]

arraySize :: Gen Int
arraySize = getSmall . getNonNegative <$> arbitrary
