{-# LANGUAGE RankNTypes #-}

module Data.KindInfer.BasicType
  ( Type(..), Name
  , Kind, KindS(..), KindVar(..), UID
  , ErrMsg
  ) where

import Control.Monad.ST.Trans

-- Type
data Type = Var   Name
          | Lit   Name
          | Alias Name [Type] Type
          -- multiple branches of product types
          | ADT   Name [Type] [[Type]]

type Name = String

-- Kind
data KindS s = Star
             | Arrow   (KindS s) (KindS s)
             | KindVar (KindVar s)

type Kind = forall s. KindS s

type KindRef s = STRef s (Maybe (KindS s))
data KindVar s = Meta UID (KindRef s)

type UID = Int

-- Other
type ErrMsg = String
