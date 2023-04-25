module Test.Data.Variant.Encodings.NestedSpec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Nested (VariantEncNested, variantFromVariantEnc, variantToVariantEnc)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type RemoteDataVarEnc =
  VariantEncNested "kind" "payload"
    ( loading :: Int
    , success :: String
    , failure ::
        { error :: String
        , errCode :: Int
        }
    )

type RemoteDataVar = Variant
  ( loading :: Int
  , success :: String
  , failure ::
      { error :: String
      , errCode :: Int
      }
  )

spec :: Spec Unit
spec =
  describe "Data.Variant.Encodings.Nested" do
    describe "toVariant" do
      it "should convert to and from Variant" do
        let
          v1 :: RemoteDataVar
          v1 = V.inj (Proxy :: _ "loading") 99

          v1' = variantToVariantEnc v1 :: VariantEncNested "kind" "payload" _

          v2 :: RemoteDataVar
          v2 = V.inj (Proxy :: _ "success") "a"

          v2' = variantToVariantEnc v2 :: VariantEncNested "kind" "payload" _

          v3 :: RemoteDataVar
          v3 = V.inj (Proxy :: _ "failure") { error: "", errCode: 0 }

          v3' = variantToVariantEnc v3 :: VariantEncNested "kind" "payload" _

        variantFromVariantEnc v1' `shouldEqual` v1
        variantFromVariantEnc v2' `shouldEqual` v2
        variantFromVariantEnc v3' `shouldEqual` v3

