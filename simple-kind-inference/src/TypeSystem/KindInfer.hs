module TypeSystem.KindInfer where

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST.Trans

import Control.Monad.KI
import Data.KindInfer.BasicType

runKindInference :: [(Name, Kind)] -> [Type] -> Either ErrMsg [Kind]
runKindInference env types = runExcept $ runST $ do
  initialEnv <- KIEnv <$> newSTRef 0 <*> pure M.empty
  flip runReaderT initialEnv $ do
    envKinds <- mapM kindToKindS [snd item | item <- env]
    let env' = zip (map fst env) envKinds
    resKinds <- extendEnv env' $ mapM inferKind types
    mapM kindSToKind resKinds

inferKind :: Type -> KI s (KindS s)
inferKind t = do
  k <- newKind
  checkKind t k
  return k

checkKind :: Type -> KindS s -> KI s ()
checkKind (Var v) k = lookupKind v >>= unify k
checkKind (Lit n) k = lookupKind n >>= unify k
checkKind (App con arg) expKind = do
  conKind <- inferKind con
  argKind <- inferKind arg
  resKind <- newKind
  unify conKind (argKind `ArrowS` resKind)
  unify expKind resKind
checkKind (Alias lhsName tvs rhs) lhsKind = do
  resKind <- newKind
  tvKinds <- mapM (const newKind) tvs
  unify lhsKind $ makeConKind resKind tvKinds
  let env = (lhsName, lhsKind) : zip tvs tvKinds
  rhsKind <- extendEnv env $ inferKind rhs
  unify rhsKind resKind
checkKind (ADT lhsName tvs rhss) lhsKind = do
  tvKinds <- mapM (const newKind) tvs
  unify lhsKind $ makeConKind StarS tvKinds
  let env = (lhsName, lhsKind) : zip tvs tvKinds
  extendEnv env $ void $ sequence $ do
    cases <- rhss
    node  <- cases
    return $ inferKind node >>= unify StarS

makeConKind :: KindS s -> [KindS s] -> KindS s
-- Given result kind k and a list of kind variables k1, k2, ..., kn
-- Return k1 -> k2 -> ... -> kn -> k
makeConKind k []     = k
makeConKind k (x:xs) = x `ArrowS` makeConKind k xs

unify :: KindS s -> KindS s -> KI s ()
unify StarS StarS = return ()
unify (a `ArrowS` b) (c `ArrowS` d) = unify a c *> unify b d
unify (KindVar kv1) (KindVar kv2) | kv1 == kv2 = return ()
unify (KindVar kv1) k2 = unifyKind kv1 k2
unify k1 (KindVar kv2) = unifyKind kv2 k1
unify _ _ = throwError "Cannot unify kinds."

unifyKind :: KindVar s -> KindS s -> KI s ()
unifyKind kv1 k2 = do
  maybeK1 <- readKindVar kv1
  case maybeK1 of
    Just k1 -> unify k1 k2
    Nothing -> assignKind kv1 k2

assignKind :: KindVar s -> KindS s -> KI s ()
assignKind kv1 (KindVar kv2) = do
  maybeK2 <- readKindVar kv2
  case maybeK2 of
    Just k2 -> unify (KindVar kv1) k2
    Nothing -> writeKindVar kv1 $ KindVar kv2
assignKind kv1 k2 = do
  if kv1 `elem` (getKindVars k2)
  then throwError "Cannot unify kinds, cycle detected."
  else writeKindVar kv1 k2
