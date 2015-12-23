module Data.KindInfer.BasicType
  ( Type(..), Name
  , Kind(..), KindS(..), KindVar(..), UID
  , ErrMsg
  ) where

import Control.Monad.ST.Trans

-- Type
data Type = Var   Name
          | Lit   Name
          | App   Type Type
          | Alias Name [Name] Type
          -- multiple branches of product types
          | ADT   Name [Name] [[Type]]

type Name = String

-- Kind
data KindS s = StarS
             | ArrowS  (KindS s) (KindS s)
             | KindVar (KindVar s)

data Kind = Star
          | Arrow Kind Kind
          deriving Eq
          
instance Show Kind where
  show Star          = "*"
  show (Arrow k1 k2) = let
    str1 = case k1 of Arrow _ _ -> "(" ++ show k1 ++  ")"
                      _         -> show k1
    str2 = show k2
    in str1 ++ " -> " ++ str2

type KindRef s = STRef s (Maybe (KindS s))
data KindVar s = Meta UID (KindRef s)
  deriving Eq

type UID = Int

-- Other
type ErrMsg = String
