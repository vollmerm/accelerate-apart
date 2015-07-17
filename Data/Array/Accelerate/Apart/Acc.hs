{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Accelerate.Apart.Acc (
  OpenAccWithName(..), OpenExpWithName, OpenFunWithName,
  accToApart, fun1ToUserFun
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
              deriving (Show)
type UserVal = (UserType, String)

data ApartFun = ApartFun
                { name   :: String
                , params :: [UserVal]
                , code   :: String
                , ret    :: [UserType]
                }
              deriving (Show)

type UserFun = ApartFun
type ArrayFun = ApartFun

data GenState = GenState
                { unique     :: Int
                , scalarDefs :: [UserFun]
                , arrayDefs  :: [ArrayFun]
                }

type Gen = State GenState

accToApart = undefined

incUnique :: Gen ()
incUnique = state $ \s -> ((), s { unique = (unique s) + 1 })

gensym :: Gen String
gensym = do
  s <- get
  let i = unique s
  incUnique
  return $ "fun" ++ (show i)

addScalarDef :: UserFun -> Gen ()
addScalarDef def = state $ \s -> ((), s { scalarDefs = (scalarDefs s) ++ [def] })

addArrayDef :: ArrayFun -> Gen ()
addArrayDef def = state $ \s -> ((), s { arrayDefs = (arrayDefs s) ++ [def] })

fun1ToUserFun :: forall t t' aenv. (Elt t, Elt t')
              => Int -> Env aenv -> OpenFun () aenv (t -> t') -> UserFun
fun1ToUserFun i aenv e@(Lam (Body _)) = do
  name <- gensym
  return $ UserFun { name = name, params = undefined, code = undefined, ret = undefined }
  where (bnds, exps) = fun1ToC aenv e
fun1ToUserFun _i _aenv _ = error "unreachable"

fun1Def :: forall t t' aenv. (Elt t, Elt t')
        => Env aenv -> OpenFun () aenv (t -> t') -> Gen UserFun
fun1Def aenv e@(Lam (Body _)) = do
  s <- get
  f <- addScalarDef $ fun1ToUserFun (unique s) aenv e
  incUnique
  return f
fun1Def _aenv _ = error "unreachable"

toVal = undefined

accGen :: forall arrs aenv. Arrays arrs
         => Env aenv -> OpenAcc aenv arrs -> Gen (OpenAccWithName aenv arrs)
accGen aenv' (OpenAcc (Alet bnd body)) = do
  bnd'  <- accGen aenv' bnd
  body' <- accGen (snd $ pushAccEnv aenv' bnd) body
  return $ OpenAccWithName noName (Alet bnd' body')
accGen aenv' acc@(OpenAcc (Map f arr)) = do
  arr' <- accGen aenv' arr
  funName <- gensym
  arrName <- gensym
  let cresTys    = accTypeToC acc
      cresNames  = accNames "res" [length cresTys - 1]
      cargTys    = accTypeToC arr
      cargNames  = accNames "arg" [length cargTys - 1]
      (bnds, es) = fun1ToC aenv' f
      funCode    = concat $ map (show . C.ppr) es
      fun        = UserFun {
        name   = funName,
        params = zipWith toVal cargNames cargTys,
        code   = funCode,
        ret    = zipWith toVal cresNames cresTys
        }
      apartArr   = ArrayFun { 
        name   = arrName,
        params = [],
        code   = "Map(" ++ funName ++ ")",
        ret    = []
        }
  addScalarDef fun 
  addArrayDef apartArr
  return undefined
accGen _aenv _ = undefined
