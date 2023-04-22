module Data.Variant.Encodings.Flat
  ( VariantEncFlat
  , class CheckCases
  , class CheckCasesRL
  , fromVariant
  , toVariant
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

foreign import data VariantEncFlat :: Symbol -> Row (Row Type) -> Type

--------------------------------------------------------------------------------
--- API
--------------------------------------------------------------------------------

toVariant
  :: forall symTag r r'
   . CheckCases symTag r r'
  => IsSymbol symTag
  => VariantEncFlat symTag r
  -> Variant r'
toVariant rec = unsafeCoerce rep
  where
  rep = VariantRep
    { type: unsafeGet (reflectSymbol prxSymTag) rec
    , value: unsafeDelete (reflectSymbol prxSymTag) rec
    }
  prxSymTag = Proxy :: _ symTag

fromVariant
  :: forall symTag r r'
   . CheckCases symTag r r'
  => IsSymbol symTag
  => Variant r'
  -> VariantEncFlat symTag r
fromVariant v =
  rep.value
    # unsafeInsert (reflectSymbol prxSymTag) rep.type

  where
  VariantRep rep = unsafeCoerce v

  prxSymTag = Proxy :: _ symTag

--------------------------------------------------------------------------------
--- CheckCases
--------------------------------------------------------------------------------

class CheckCases :: Symbol -> Row (Row Type) -> Row Type -> Constraint
class CheckCases symTag r1 r2 | symTag r1 -> r2

instance (RowToList r1 rl1, CheckCasesRL symTag rl1 r2) => CheckCases symTag r1 r2

--------------------------------------------------------------------------------
--- CheckCasesRL
--------------------------------------------------------------------------------

class CheckCasesRL :: Symbol -> RowList (Row Type) -> Row Type -> Constraint
class CheckCasesRL symTag rl1 r2 | symTag rl1 -> r2

instance CheckCasesRL symTag RL.Nil ()

instance
  ( Row.Cons sym (Record r) r2' r2
  , Row.Lacks symTag r2
  , CheckCasesRL symTag rl r2'
  ) =>
  CheckCasesRL symTag (RL.Cons sym r rl) r2

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import unsafeGet :: forall a b. String -> a -> b

foreign import unsafeDelete :: forall a b. String -> a -> b

foreign import unsafeInsert :: forall a b b'. String -> a -> b -> b'
