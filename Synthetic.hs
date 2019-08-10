{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Synthetic where

import Control.Comonad
import Data.Foldable
import Data.Functor.Const
import GHC.Generics

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Data.Functor.Foldable (Fix)
import Data.Set (Set)

import qualified Data.Set as Set
import qualified Data.Functor.Foldable as Recursive

-- * Synthetic attributes

{- | Synthetic attributes over a tree described by a base functor.

See also: <https://en.wikipedia.org/wiki/Attribute_grammar#Synthesized_attributes>
 -}
class Functor base => Synthetic syn base where
  -- | Synthesize an attribute by descending one level in the tree.
  synthetic :: base syn -> syn

-- * Generic instances of Synthetic

instance (Functor base, Synthetic syn base) => Synthetic syn (M1 i c base) where
  synthetic = synthetic . unM1

instance (Functor base, Synthetic syn base) => Synthetic syn (Rec1 base) where
  synthetic = synthetic . unRec1

instance
  (Functor left, Synthetic syn left, Functor right, Synthetic syn right)
  => Synthetic syn (left :+: right) where
  synthetic (L1 lsyn) = synthetic lsyn
  synthetic (R1 rsyn) = synthetic rsyn

{- | A wrapper on which to dispatch Generic1-derived instances.

We would rather not provide a default Generic1 implementation of synthetic
because it is valid only for "mere" aggregation types, and that would set a
dangerous trap.

Instead, we will provide a Generic1 instance for Generically1, which we use
with DerivingVia.

 -}
newtype Generically1 functor a =
  Generically1 { unGenerically1 :: functor a }
  deriving Functor

instance
  (Functor functor, Generic1 functor, Synthetic syn (Rep1 functor)) =>
  Synthetic syn (Generically1 functor) where
  synthetic = synthetic . from1 . unGenerically1

-- * Example: Free variables

type Name = String

-- | The set of free variables of a term is a synthetic attribute.
newtype FreeVars = FreeVars { getFreeVars :: Set Name }
  deriving (Semigroup, Monoid) via (Set Name)

freeVar :: Name -> FreeVars
freeVar = FreeVars . Set.singleton

bindVar :: Name -> FreeVars -> FreeVars
bindVar bound = FreeVars . Set.delete bound . getFreeVars

-- * λ-calculus

newtype Var = Var Name

data Abs child = Abs Name child
  deriving (Functor, Foldable, Traversable)

data App child = App child child
  deriving (Functor, Foldable, Traversable)

instance Synthetic FreeVars (Const Var) where
  synthetic (Const (Var name)) = freeVar name

instance Synthetic FreeVars Abs where
  synthetic (Abs bound child) = bindVar bound child

instance Synthetic FreeVars App where
  synthetic = fold

data LamF child
  = VarF (Const Var child)
  | AbsF (Abs child)
  | AppF (App child)
  deriving (Functor, Foldable, Traversable)
  deriving (Generic, Generic1)
  deriving (Synthetic FreeVars) via (Generically1 LamF)

-- | Fold up an expression to find its free variables.
freeVars :: Fix LamF -> FreeVars
freeVars = Recursive.fold synthetic

-- | Annotate an expression so its free variables are always handy.
annotate :: Fix LamF -> Cofree LamF FreeVars
annotate lam = Recursive.embed (syn :< lamF)
  where
    lamF = annotate <$> Recursive.project lam
    syn = synthetic (extract <$> lamF)
