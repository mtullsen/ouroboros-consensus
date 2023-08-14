{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

-- | Type-level non-empty lists
module Data.SOP.Strict.NonEmpty (
    IsNonEmpty (..)
  , ProofNonEmpty (..)
  , checkIsNonEmpty
  ) where

import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.Sing

type ProofNonEmpty :: [a] -> Type
data ProofNonEmpty xs where
  ProofNonEmpty :: !(Proxy x) -> !(Proxy xs) -> ProofNonEmpty (x ': xs)

class IsNonEmpty xs where
  isNonEmpty :: proxy xs -> ProofNonEmpty xs

instance IsNonEmpty (x ': xs) where
  isNonEmpty _ = ProofNonEmpty (Proxy @x) (Proxy @xs)

checkIsNonEmpty :: forall xs. SListI xs => Proxy xs -> Maybe (ProofNonEmpty xs)
checkIsNonEmpty _ = case sList @xs of
    SNil  -> Nothing
    SCons -> Just $ ProofNonEmpty Proxy Proxy
