module Data.Variant.Encodings.Nested
  ( VariantEncNested
  , fromVariant
  , toVariant
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data VariantEncNested :: Symbol -> Symbol -> Row Type -> Type

toVariant
  :: forall symTag symVal r
   . IsSymbol symTag
  => IsSymbol symVal
  => VariantEncNested symTag symVal r
  -> Variant r
toVariant rec = unsafeCoerce rep
  where
  rep = VariantRep
    { type: unsafeGet (reflectSymbol prxSymTag) rec
    , value: unsafeGet (reflectSymbol prxSymVal) rec
    }
  prxSymTag = Proxy :: _ symTag
  prxSymVal = Proxy :: _ symVal

fromVariant
  :: forall symTag symVal r
   . IsSymbol symTag
  => IsSymbol symVal
  => Variant r
  -> VariantEncNested symTag symVal r
fromVariant v =
  {}
    # unsafeInsert (reflectSymbol prxSymTag) rep.type
    # unsafeInsert (reflectSymbol prxSymVal) rep.value
  where
  VariantRep rep = unsafeCoerce v

  prxSymTag = Proxy :: _ symTag
  prxSymVal = Proxy :: _ symVal

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import unsafeGet :: forall a b. String -> a -> b

foreign import unsafeInsert :: forall a b b'. String -> a -> b -> b'
