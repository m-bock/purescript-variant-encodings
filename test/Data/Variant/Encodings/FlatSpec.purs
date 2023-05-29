module Test.Data.Variant.Encodings.FlatSpec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Flat (VariantEncodedFlat, normalizeEncodingFlat, customizeEncodingFlat)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type RemoteDataVarEnc =
  VariantEncodedFlat "kind"
    ( loading :: { progress :: Int }
    , success :: { result :: String }
    , failure ::
        { error :: String
        , errCode :: Int
        }
    )

type RemoteDataVar = Variant
  ( loading :: { progress :: Int }
  , success :: { result :: String }
  , failure ::
      { error :: String
      , errCode :: Int
      }
  )

spec :: Spec Unit
spec =
  describe "Data.Variant.Encodings.Flat" do
    describe "normalizeEncodingFlat" do
      it "should convert to and from Variant" do
        let
          v1 :: RemoteDataVar
          v1 = V.inj (Proxy :: _ "loading") { progress: 99 }

          v1' = customizeEncodingFlat v1 :: VariantEncodedFlat "kind" _

          v2 :: RemoteDataVar
          v2 = V.inj (Proxy :: _ "success") { result: "a" }

          v2' = customizeEncodingFlat v2 :: VariantEncodedFlat "kind" _

          v3 :: RemoteDataVar
          v3 = V.inj (Proxy :: _ "failure") { error: "", errCode: 0 }

          v3' = customizeEncodingFlat v3 :: VariantEncodedFlat "kind" _

        normalizeEncodingFlat v1' `shouldEqual` v1
        normalizeEncodingFlat v2' `shouldEqual` v2
        normalizeEncodingFlat v3' `shouldEqual` v3

