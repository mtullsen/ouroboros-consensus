{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |

module Data.SOP.NonEmpty (
    -- * Type-level non-empty lists
    IsNonEmpty (..)
  , ProofNonEmpty (..)
  , checkIsNonEmpty
    -- * Type-level empty lists
  , IsEmpty (..)
  , ProofEmpty (..)
  , checkIsEmpty
    -- *  Empty or non-empty level lists
  , checkEmptiness
  ) where

import           Data.Kind (Type)
import           Data.SOP.Strict

{-------------------------------------------------------------------------------
  Type-level non-empty lists
-------------------------------------------------------------------------------}

data ProofNonEmpty :: [a] -> Type where
  ProofNonEmpty :: Proxy x -> Proxy xs -> ProofNonEmpty (x ': xs)

class IsNonEmpty xs where
  isNonEmpty :: proxy xs -> ProofNonEmpty xs

instance IsNonEmpty (x ': xs) where
  isNonEmpty _ = ProofNonEmpty (Proxy @x) (Proxy @xs)

checkIsNonEmpty :: forall xs. SListI xs => Proxy xs -> Maybe (ProofNonEmpty xs)
checkIsNonEmpty _ = case sList @xs of
    SNil  -> Nothing
    SCons -> Just $ ProofNonEmpty Proxy Proxy

{-------------------------------------------------------------------------------
  Type-level non-empty lists
-------------------------------------------------------------------------------}

data ProofEmpty :: [a] -> Type where
  ProofEmpty :: ProofEmpty '[]

class IsEmpty xs where
  isEmpty :: Proxy xs -> ProofEmpty xs

instance IsEmpty '[] where
  isEmpty _ = ProofEmpty

checkIsEmpty :: forall xs. SListI xs => Proxy xs -> Maybe (ProofEmpty xs)
checkIsEmpty _ = case sList @xs of
    SNil  -> Just ProofEmpty
    SCons -> Nothing

{-------------------------------------------------------------------------------
  Type-level non-empty or empty lists
-------------------------------------------------------------------------------}

checkEmptiness ::
     forall xs. SListI xs
  => Proxy xs
  -> Either (ProofEmpty xs) (ProofNonEmpty xs)
checkEmptiness _ = case sList @xs of
    SNil  -> Left ProofEmpty
    SCons -> Right $ ProofNonEmpty Proxy Proxy
