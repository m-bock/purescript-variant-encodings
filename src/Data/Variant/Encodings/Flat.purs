module Data.Variant.Encodings.Flat
  ( VariantEncodedFlat
  , class CheckCases
  , class CheckCasesRL
  , class IsRecordWithoutKey
  , class IsVariantEncodedFlat
  , normalizeEncodingFlat
  , normalizeEncodingFlat'
  , customizeEncodingFlat
  , customizeEncodingFlat'
  , isRecordWithoutKey
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

foreign import data VariantEncodedFlat :: Symbol -> Row Type -> Type

--------------------------------------------------------------------------------
--- API
--------------------------------------------------------------------------------

class IsVariantEncodedFlat symTag rowVarEnc rowVar | symTag rowVar -> rowVarEnc where
  customizeEncodingFlat :: Variant rowVar -> VariantEncodedFlat symTag rowVarEnc
  normalizeEncodingFlat :: VariantEncodedFlat symTag rowVarEnc -> Variant rowVar

instance (IsSymbol symTag, CheckCases symTag rowVarEnc rowVar) => IsVariantEncodedFlat symTag rowVarEnc rowVar where

  customizeEncodingFlat v =
    rep.value
      # unsafeInsert (reflectSymbol prxSymTag) rep.type

    where
    VariantRep rep = unsafeCoerce v

    prxSymTag = Proxy :: _ symTag

  normalizeEncodingFlat rec = unsafeCoerce rep
    where
    rep = VariantRep
      { type: unsafeGet (reflectSymbol prxSymTag) rec
      , value: unsafeDelete (reflectSymbol prxSymTag) rec
      }
    prxSymTag = Proxy :: _ symTag

--------------------------------------------------------------------------------
--- Proxy API
--------------------------------------------------------------------------------

normalizeEncodingFlat'
  :: forall symTag rowVarEnc rowVar
   . IsVariantEncodedFlat symTag rowVarEnc rowVar
  => Proxy (VariantEncodedFlat symTag rowVarEnc)
  -> Proxy (Variant rowVar)
normalizeEncodingFlat' _ = Proxy

customizeEncodingFlat'
  :: forall symTag rowVarEnc rowVar
   . IsVariantEncodedFlat symTag rowVarEnc rowVar
  => Proxy symTag
  -> Proxy (Variant rowVar)
  -> Proxy (VariantEncodedFlat symTag rowVarEnc)
customizeEncodingFlat' _ _ = Proxy

--------------------------------------------------------------------------------
--- CheckCases
--------------------------------------------------------------------------------

class CheckCases :: Symbol -> Row Type -> Row Type -> Constraint
class CheckCases symTag rowVarEnc rowVar | symTag rowVar -> rowVarEnc

instance
  ( RowToList rowVar rlVar
  , CheckCasesRL symTag rlVar rowVarEnc
  ) =>
  CheckCases symTag rowVarEnc rowVar

--------------------------------------------------------------------------------
--- CheckCasesRL
--------------------------------------------------------------------------------

class CheckCasesRL :: Symbol -> RowList Type -> Row Type -> Constraint
class CheckCasesRL symTag rlVar rowVarEnc | symTag rlVar -> rowVarEnc

instance CheckCasesRL symTag RL.Nil ()

instance
  ( Row.Cons sym a rowVarEncPrev rowVarEnc
  , IsRecordWithoutKey symTag a
  , CheckCasesRL symTag rlVar rowVarEncPrev
  ) =>
  CheckCasesRL symTag (RL.Cons sym a rlVar) rowVarEnc

--------------------------------------------------------------------------------
--- IsRecordWithoutKey
--------------------------------------------------------------------------------

class IsRecordWithoutKey :: forall k. Symbol -> k -> Constraint
class
  IsRecordWithoutKey sym r
  where
  isRecordWithoutKey :: Proxy sym -> Proxy r

instance
  ( Row.Lacks sym r
  ) =>
  IsRecordWithoutKey sym (Record r) where
  isRecordWithoutKey _ = Proxy

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import unsafeGet :: forall a b. String -> a -> b

foreign import unsafeDelete :: forall a b. String -> a -> b

foreign import unsafeInsert :: forall a b b'. String -> a -> b -> b'
