module Control.Monad.KI
  ( KIEnv(..), KI
  , extendEnv, lookupKind
  , kindToKindS, kindSToKind
  , newKind
  , readKindVar, writeKindVar
  ) where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST.Trans

import Data.KindInfer.BasicType

-- Type Definition
data KIEnv s = KIEnv
  { counter :: STRef s UID
  , getEnv :: M.Map Name (KindS s)
  }

type KI s = ReaderT (KIEnv s) (STT s (Except ErrMsg))

-- Monad-related auxiliaries
newKIRef :: a -> KI s (STRef s a)
newKIRef = lift . newSTRef

readKIRef :: STRef s a -> KI s a
readKIRef = lift . readSTRef

writeKIRef :: STRef s a -> a -> KI s ()
writeKIRef ref val = lift $ writeSTRef ref val

readKindVar :: KindVar s -> KI s (Maybe (KindS s))
readKindVar (Meta _ ref) = readKIRef ref

writeKindVar :: KindVar s -> KindS s -> KI s ()
writeKindVar (Meta _ ref) kind = writeKIRef ref $ Just kind

-- Environment Manipulation
extendEnv :: [(Name, KindS s)] -> KI s a -> KI s a
extendEnv pairs m = fmap updateEnv ask >>= lift . runReaderT m where
  updateEnv (KIEnv cnt env) = KIEnv cnt $ M.union (M.fromList pairs) env

lookupKind :: Name -> KI s (KindS s)
lookupKind n = do
  env <- fmap getEnv ask
  case M.lookup n env of
    Just k  -> return k
    Nothing -> throwError $ "Not in scope: " ++ show n

-- KindS <-> Kind
kindToKindS :: Kind -> KI s (KindS s)
kindToKindS Star            = return StarS
kindToKindS (k1 `Arrow` k2) = ArrowS <$> kindToKindS k1 <*> kindToKindS k2

kindSToKind :: KindS s -> KI s Kind
kindSToKind StarS          = return Star
kindSToKind (ArrowS k1 k2) = Arrow <$> kindSToKind k1 <*> kindSToKind k2
kindSToKind (KindVar kv)   = do
  maybeK <- readKindVar kv
  case maybeK of
    Just k  -> kindSToKind k
    Nothing -> return Star

-- Create kind variables
newKind :: KI s (KindS s)
newKind = fmap KindVar newKindVar

newKindVar :: KI s (KindVar s)
newKindVar = Meta <$> newUID <*> newRef where
  newUID = do
    ref  <- counter <$> ask
    uniq <- readKIRef ref
    writeKIRef ref (uniq + 1)
    return uniq

  newRef = newKIRef Nothing
