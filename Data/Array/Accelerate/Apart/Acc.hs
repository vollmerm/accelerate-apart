{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Accelerate.Apart.Acc (
  OpenAccWithName(..), OpenExpWithName, OpenFunWithName,
  accToApart, funToUserFun
  ) where

import           Control.Monad.State
import           Data.List
import qualified Language.C                        as C
import           Language.C.Quote.C                as C

import           Data.Array.Accelerate.Array.Sugar
import           Data.Array.Accelerate.AST         hiding (Val (..), prj)
import           Data.Array.Accelerate.Tuple

import           Data.Array.Accelerate.Apart.Base
import           Data.Array.Accelerate.Apart.Exp
import           Data.Array.Accelerate.Apart.Type

data OpenAccWithName aenv t = OpenAccWithName Name (PreOpenAcc OpenAccWithName aenv t)
type OpenExpWithName = PreOpenExp OpenAccWithName
type OpenFunWithName = PreOpenFun OpenAccWithName

data UserType = Fl | Fx | Tup [UserType]
type UserVal = (UserType, String)

data UserFun = UserFun
               { params     :: [UserVal]
               , code       :: [C.Exp]
               , returnType :: UserVal
               }

data GenState = GenState
                { unique :: Int
                , defs   :: [UserFun]
                }

accToApart = undefined
funToUserFun = undefined
