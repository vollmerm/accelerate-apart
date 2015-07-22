{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Accelerate.Apart.Acc (
  OpenAccWithName(..), OpenExpWithName, OpenFunWithName,
  accToApart, accGen
  ) where

import           Control.Monad.State
import           Data.List
import qualified Text.PrettyPrint.Mainland as C
import qualified Language.C as C
import           Language.C.Quote.C as C

import           Data.Array.Accelerate.AST hiding (Val (..), prj)
import           Data.Array.Accelerate.Array.Sugar
import           Data.Array.Accelerate.Trafo.Sharing as Sharing
import           Data.Array.Accelerate.Tuple

import           Data.Array.Accelerate.Apart.Base
import           Data.Array.Accelerate.Apart.Exp
import           Data.Array.Accelerate.Apart.Type

data OpenAccWithName aenv t = OpenAccWithName Name (PreOpenAcc OpenAccWithName aenv t)
type OpenExpWithName = PreOpenExp OpenAccWithName
type OpenFunWithName = PreOpenFun OpenAccWithName

type UserVal = (String, String)

data UserFun = UserFun
               { usrName   :: String
               , usrParams :: [UserVal]
               , usrCode   :: String
               , usrRet    :: [UserVal]
               }
               deriving (Show)

data ArrayFun = ArrayFun
                { arrName :: String
                , arrCode :: String
                }
                deriving (Show)

data GenState = GenState
                { unique     :: Int
                , scalarDefs :: [UserFun]
                , arrayDefs  :: [ArrayFun]
                }

type Gen = State GenState

accToApart :: forall arrs aenv. Arrays arrs => Env aenv -> OpenAcc aenv arrs
           -> (String, String, OpenAccWithName aenv arrs)
accToApart aenv acc = let (acc', state) = runState (accGen aenv acc) $ GenState 0 [] []
                          userFuns      = concat $ map show (scalarDefs state)
                          arrayFuns     = concat $ map show (arrayDefs state)
                      in (userFuns, arrayFuns, acc')

incUnique :: Gen ()
incUnique = state $ \s -> ((), s { unique = (unique s) + 1 })

gensym :: Gen String
gensym = do
  s <- get
  let i = unique s
  incUnique
  return $ "gensym" ++ (show i)

addScalarDef :: UserFun -> Gen ()
addScalarDef def = state $ \s -> ((), s { scalarDefs = (scalarDefs s) ++ [def] })

addArrayDef :: ArrayFun -> Gen ()
addArrayDef def = state $ \s -> ((), s { arrayDefs = (arrayDefs s) ++ [def] })

accGen :: forall arrs aenv. Arrays arrs
         => Env aenv -> OpenAcc aenv arrs -> Gen (OpenAccWithName aenv arrs)
accGen aenv' (OpenAcc (Alet bnd body)) = do
  bnd'  <- accGen aenv' bnd
  body' <- accGen (snd $ pushAccEnv aenv' bnd) body
  return $ OpenAccWithName noName (Alet bnd' body')
accGen aenv' (OpenAcc (Avar ix)) = return $ OpenAccWithName noName (Avar ix)
accGen aenv' (OpenAcc (Use arr)) = return $ OpenAccWithName noName (Use arr)
accGen aenv' acc@(OpenAcc (Map f arr)) = do
  arr' <- accGen aenv' arr
  funName <- gensym
  arrName <- gensym
  lambName <- gensym
  let cresTys    = accTypeToC acc
      cresNames  = accNames "res" [length cresTys - 1]
      cargTys    = accTypeToC arr
      cargNames  = accNames "arg" [length cargTys - 1]
      (bnds, es) = fun1ToC aenv' f
      funCode    = concat $ map (show . C.ppr) es
      fun        = UserFun {
        usrName   = funName,
        usrParams = zip cargNames $ map (show . C.ppr) cargTys,
        usrCode   = funCode,
        usrRet    = zip cresNames $ map (show . C.ppr) cresTys
        }
      apartArr   = ArrayFun {
        arrName   = arrName,
        arrCode   = "(fun " ++ lambName ++ " => Map(" ++ funName ++ ") $ " ++ lambName ++ ")"
        }

  addScalarDef fun
  addArrayDef apartArr
  return $ OpenAccWithName funName (Map (adaptFun f) arr')
accGen _aenv _ = error "Not implemented."

adaptFun :: OpenFun env aenv t -> OpenFunWithName env aenv t
adaptFun (Body e) = Body $ adaptExp e
adaptFun (Lam  f) = Lam  $ adaptFun f
adaptExp :: OpenExp env aenv t -> OpenExpWithName env aenv t
adaptExp e = case e of
              Var ix -> Var ix
              Let bnd body -> Let (adaptExp bnd) (adaptExp body)
              Const c -> Const c
              PrimConst c -> PrimConst c
              PrimApp f x -> PrimApp f (adaptExp x)
              Tuple t -> Tuple (adaptTuple t)
              Prj ix e -> Prj ix (adaptExp e)
              Cond p t e -> Cond (adaptExp p) (adaptExp t) (adaptExp e)
              IndexAny -> IndexAny
              IndexNil -> IndexNil
              IndexCons sh sz -> IndexCons (adaptExp sh) (adaptExp sz)
              IndexHead sh -> IndexHead (adaptExp sh)
              IndexTail sh -> IndexTail (adaptExp sh)
              IndexSlice ix slix sh -> IndexSlice ix (adaptExp slix) (adaptExp sh)
              IndexFull ix slix sl -> IndexFull ix (adaptExp slix) (adaptExp sl)
              ToIndex sh ix -> ToIndex (adaptExp sh) (adaptExp ix)
              FromIndex sh ix -> FromIndex (adaptExp sh) (adaptExp ix)
              Intersect sh1 sh2 -> Intersect (adaptExp sh1) (adaptExp sh2)
              ShapeSize sh -> ShapeSize (adaptExp sh)
              Shape acc -> Shape (adaptAcc acc)
              Index acc ix -> Index (adaptAcc acc) (adaptExp ix)
              LinearIndex acc ix -> LinearIndex (adaptAcc acc) (adaptExp ix)
              Foreign fo f x -> Foreign fo (adaptFun f) (adaptExp x)
  where
    adaptTuple :: Tuple (OpenExp env aenv) t -> Tuple (OpenExpWithName env aenv) t
    adaptTuple NilTup = NilTup
    adaptTuple (t `SnocTup` e) = adaptTuple t `SnocTup` adaptExp e
    adaptAcc (OpenAcc (Avar ix)) = OpenAccWithName noName (Avar ix)
    adaptAcc _ = error "D.A.A.C: unlifted array computation"
