module Data.Variant.Encodings.Nested
  ( VariantEncodedNested
  , normalizeEncodingNested
  , customizeEncodingNested
  , normalizeEncodingNested'
  , customizeEncodingNested'
  , class IsVariantEncodedNested

  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

foreign import data VariantEncodedNested :: Symbol -> Symbol -> Row Type -> Type

--------------------------------------------------------------------------------
--- API
--------------------------------------------------------------------------------

class IsVariantEncodedNested symTag symVal rowVarEnc rowVar | symTag symVal rowVar -> rowVarEnc where
  customizeEncodingNested :: Variant rowVar -> VariantEncodedNested symTag symVal rowVarEnc
  normalizeEncodingNested :: VariantEncodedNested symTag symVal rowVarEnc -> Variant rowVar

instance (IsSymbol symTag, IsSymbol symVal) => IsVariantEncodedNested symTag symVal rowVarEnc rowVar where

  customizeEncodingNested v =
    {}
      # unsafeInsert (reflectSymbol prxSymTag) rep.type
      # unsafeInsert (reflectSymbol prxSymVal) rep.value
    where
    VariantRep rep = unsafeCoerce v

    prxSymTag = Proxy :: _ symTag
    prxSymVal = Proxy :: _ symVal

  normalizeEncodingNested rec = unsafeCoerce rep
    where
    rep = VariantRep
      { type: unsafeGet (reflectSymbol prxSymTag) rec
      , value: unsafeGet (reflectSymbol prxSymVal) rec
      }
    prxSymTag = Proxy :: _ symTag
    prxSymVal = Proxy :: _ symVal

--------------------------------------------------------------------------------
--- Proxy API
--------------------------------------------------------------------------------

normalizeEncodingNested'
  :: forall symTag symVal r
   . Proxy (VariantEncodedNested symTag symVal r)
  -> Proxy (Variant r)
normalizeEncodingNested' _ = Proxy

customizeEncodingNested'
  :: forall symTag symVal r
   . Proxy (Variant r)
  -> Proxy (VariantEncodedNested symTag symVal r)
customizeEncodingNested' _ = Proxy

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import unsafeGet :: forall a b. String -> a -> b

foreign import unsafeInsert :: forall a b b'. String -> a -> b -> b'
