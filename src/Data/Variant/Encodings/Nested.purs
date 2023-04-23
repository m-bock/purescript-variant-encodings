module Data.Variant.Encodings.Nested
  ( VariantEncNested
  , fromVariant
  , fromVariant'
  , toVariant
  , toVariant'
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

toVariant'
  :: forall symTag symVal r
   . Proxy (VariantEncNested symTag symVal r)
  -> Proxy (Variant r)
toVariant' _ = Proxy

fromVariant
  :: forall symTag symVal r
   . IsSymbol symTag
  => IsSymbol symVal
  => Proxy symTag
  -> Proxy symVal
  -> Variant r
  -> VariantEncNested symTag symVal r
fromVariant _ _ v =
  {}
    # unsafeInsert (reflectSymbol prxSymTag) rep.type
    # unsafeInsert (reflectSymbol prxSymVal) rep.value
  where
  VariantRep rep = unsafeCoerce v

  prxSymTag = Proxy :: _ symTag
  prxSymVal = Proxy :: _ symVal

fromVariant'
  :: forall symTag symVal r
   . Proxy symTag
  -> Proxy symVal
  -> Proxy (Variant r)
  -> Proxy (VariantEncNested symTag symVal r)
fromVariant' _ _ _ = Proxy

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import unsafeGet :: forall a b. String -> a -> b

foreign import unsafeInsert :: forall a b b'. String -> a -> b -> b'
