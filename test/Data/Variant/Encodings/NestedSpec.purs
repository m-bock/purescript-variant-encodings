module Test.Data.Variant.Encodings.NestedSpec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Nested (VariantEncNested, fromVariant, toVariant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type RemoteData =
  VariantEncNested "kind" "payload"
    ( loading :: Int
    , success :: String
    , failure ::
        { error :: String
        , errCode :: Int
        }
    )

type RemoteData' = Variant
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
          v1 :: RemoteData'
          v1 = V.inj (Proxy :: _ "loading") 99

          v1' :: RemoteData
          v1' = fromVariant v1

          v2 :: RemoteData'
          v2 = V.inj (Proxy :: _ "success") "a"

          v2' :: RemoteData
          v2' = fromVariant v2

          v3 :: RemoteData'
          v3 = V.inj (Proxy :: _ "failure") { error: "", errCode: 0 }

          v3' :: RemoteData
          v3' = fromVariant v3

        toVariant v1' `shouldEqual` v1
        toVariant v2' `shouldEqual` v2
        toVariant v3' `shouldEqual` v3

