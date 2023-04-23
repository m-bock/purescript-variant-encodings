module Test.Data.Variant.Encodings.NestedSpec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Nested (VariantEncNested, fromVariant, toVariant)
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

          v1' = fromVariant (Proxy :: _ "kind") (Proxy :: _ "payload") v1

          v2 :: RemoteDataVar
          v2 = V.inj (Proxy :: _ "success") "a"

          v2' = fromVariant (Proxy :: _ "kind") (Proxy :: _ "payload") v2

          v3 :: RemoteDataVar
          v3 = V.inj (Proxy :: _ "failure") { error: "", errCode: 0 }

          v3' = fromVariant (Proxy :: _ "kind") (Proxy :: _ "payload") v3

        toVariant v1' `shouldEqual` v1
        toVariant v2' `shouldEqual` v2
        toVariant v3' `shouldEqual` v3

