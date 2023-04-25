module Data.Variant.Encodings.Nested
  ( VariantEncNested
  , variantFromVariantEnc
  , variantToVariantEnc
  , variantFromVariantEnc'
  , variantToVariantEnc'
  , class IsVariantEncNested

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

foreign import data VariantEncNested :: Symbol -> Symbol -> Row Type -> Type

--------------------------------------------------------------------------------
--- API
--------------------------------------------------------------------------------

class IsVariantEncNested symTag symVal rowVarEnc rowVar | symTag symVal rowVar -> rowVarEnc where
  variantToVariantEnc :: Variant rowVar -> VariantEncNested symTag symVal rowVarEnc
  variantFromVariantEnc :: VariantEncNested symTag symVal rowVarEnc -> Variant rowVar

instance (IsSymbol symTag, IsSymbol symVal) => IsVariantEncNested symTag symVal rowVarEnc rowVar where

  variantToVariantEnc v =
    {}
      # unsafeInsert (reflectSymbol prxSymTag) rep.type
      # unsafeInsert (reflectSymbol prxSymVal) rep.value
    where
    VariantRep rep = unsafeCoerce v

    prxSymTag = Proxy :: _ symTag
    prxSymVal = Proxy :: _ symVal

  variantFromVariantEnc rec = unsafeCoerce rep
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

variantFromVariantEnc'
  :: forall symTag symVal r
   . Proxy (VariantEncNested symTag symVal r)
  -> Proxy (Variant r)
variantFromVariantEnc' _ = Proxy

variantToVariantEnc'
  :: forall symTag symVal r
   . Proxy (Variant r)
  -> Proxy (VariantEncNested symTag symVal r)
variantToVariantEnc' _ = Proxy

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import unsafeGet :: forall a b. String -> a -> b

foreign import unsafeInsert :: forall a b b'. String -> a -> b -> b'
