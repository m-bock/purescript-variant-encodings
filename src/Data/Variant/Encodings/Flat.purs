module Data.Variant.Encodings.Flat
  ( VariantEncFlat
  , class CheckCases
  , class CheckCasesRL
  , fromVariant
  , fromVariant'
  , toVariant
  , toVariant'
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
  :: forall symTag rowVarEnc rowVar
   . CheckCases symTag rowVarEnc rowVar
  => IsSymbol symTag
  => VariantEncFlat symTag rowVarEnc
  -> Variant rowVar
toVariant rec = unsafeCoerce rep
  where
  rep = VariantRep
    { type: unsafeGet (reflectSymbol prxSymTag) rec
    , value: unsafeDelete (reflectSymbol prxSymTag) rec
    }
  prxSymTag = Proxy :: _ symTag

toVariant'
  :: forall symTag rowVarEnc rowVar
   . CheckCases symTag rowVarEnc rowVar
  => IsSymbol symTag
  => Proxy (VariantEncFlat symTag rowVarEnc)
  -> Proxy (Variant rowVar)
toVariant' _ = Proxy

fromVariant
  :: forall symTag rowVarEnc rowVar
   . CheckCases symTag rowVarEnc rowVar
  => IsSymbol symTag
  => Proxy symTag
  -> Variant rowVar
  -> VariantEncFlat symTag rowVarEnc
fromVariant _ v =
  rep.value
    # unsafeInsert (reflectSymbol prxSymTag) rep.type

  where
  VariantRep rep = unsafeCoerce v

  prxSymTag = Proxy :: _ symTag

fromVariant'
  :: forall symTag rowVarEnc rowVar
   . CheckCases symTag rowVarEnc rowVar
  => IsSymbol symTag
  => Proxy symTag
  -> Proxy (Variant rowVar)
  -> Proxy (VariantEncFlat symTag rowVarEnc)
fromVariant' _ _ = Proxy

--------------------------------------------------------------------------------
--- CheckCases
--------------------------------------------------------------------------------

class CheckCases :: Symbol -> Row (Row Type) -> Row Type -> Constraint
class CheckCases symTag rowVarEnc rowVar | symTag rowVar -> rowVarEnc

instance
  ( RowToList rowVar rlVar
  , CheckCasesRL symTag rlVar rowVarEnc
  ) =>
  CheckCases symTag rowVarEnc rowVar

--------------------------------------------------------------------------------
--- CheckCasesRL
--------------------------------------------------------------------------------

class CheckCasesRL :: Symbol -> RowList Type -> Row (Row Type) -> Constraint
class CheckCasesRL symTag rlVar rowVarEnc | symTag rlVar -> rowVarEnc

instance CheckCasesRL symTag RL.Nil ()

instance
  ( Row.Cons sym r rowVarEncPrev rowVarEnc
  , Row.Lacks symTag rowVarEnc
  , CheckCasesRL symTag rlVar rowVarEncPrev
  ) =>
  CheckCasesRL symTag (RL.Cons sym (Record r) rlVar) rowVarEnc

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import unsafeGet :: forall a b. String -> a -> b

foreign import unsafeDelete :: forall a b. String -> a -> b

foreign import unsafeInsert :: forall a b b'. String -> a -> b -> b'
