{-# LANGUAGE InstanceSigs #-}
module Lambda.Index (
    Index,
    Ctx, emptyCtx,
    getIndex,
    backData,
    addData,
    exist,
    add, lt, isucc,
    ofInt) where

import Lambda.Ident (Name)

import Data.List (elemIndex)

import Fmt ((+||), (||+))

data Index = MkIndex {
    index :: Int,
    name :: Maybe String
}

instance Eq Index where
    (==) :: Index -> Index -> Bool
    (==) (MkIndex { index = i1}) (MkIndex {index = i2}) = i1 == i2

instance Show Index where
    show :: Index -> String
    show (MkIndex { index, name}) = case name of
        Just name' -> name'
        Nothing -> show index

add :: Int -> Index -> Index
add n (MkIndex {index, name}) = MkIndex {index = index + n, name }

lt :: Index -> Index -> Bool
lt (MkIndex { index = i1 }) (MkIndex { index = i2 }) = i1 < i2

isucc :: Index -> Index
isucc = add 1

ofInt :: Int -> Index
ofInt n = MkIndex { index = n, name = Nothing }

newtype Ctx a = MkCtx [a] deriving (Show)

emptyCtx :: Ctx a
emptyCtx = MkCtx []

getIndex :: Eq a => a -> Name -> Ctx a -> Index
getIndex item name (MkCtx ctx) = case elemIndex item ctx of
        Just index -> MkIndex { index, name = Just name }
        Nothing     -> error $ "undefined reference: " ++ name

backData :: Index -> Ctx a -> a
backData (MkIndex { index, name }) (MkCtx ls) =
    if index >= length ls
    then error $ "out of bound lookup on "+||name||+"."
    else ls !! index

addData :: a -> Ctx a -> Ctx a
addData item (MkCtx ls) = MkCtx (item : ls)

exist :: Eq a => a -> Ctx a -> Bool
exist item (MkCtx ls) = item `elem` ls
