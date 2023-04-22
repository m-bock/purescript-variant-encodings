module Test.Data.Variant.Encodings.FlatSpec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Flat (VariantEncFlat, fromVariant, toVariant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type RemoteData =
  VariantEncFlat "kind"
    ( loading :: (progress :: Int)
    , success :: (result :: String)
    , failure ::
        ( error :: String
        , errCode :: Int
        )
    )

type RemoteData' = Variant
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
    describe "toVariant" do
      it "should convert to and from Variant" do
        let
          v1 :: RemoteData'
          v1 = V.inj (Proxy :: _ "loading") { progress: 99 }

          v1' :: RemoteData
          v1' = fromVariant v1

          v2 :: RemoteData'
          v2 = V.inj (Proxy :: _ "success") { result: "a" }

          v2' :: RemoteData
          v2' = fromVariant v2

          v3 :: RemoteData'
          v3 = V.inj (Proxy :: _ "failure") { error: "", errCode: 0 }

          v3' :: RemoteData
          v3' = fromVariant v3

        toVariant v1' `shouldEqual` v1
        toVariant v2' `shouldEqual` v2
        toVariant v3' `shouldEqual` v3

