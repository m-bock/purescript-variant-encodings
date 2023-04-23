module Test.Data.Variant.Encodings.FlatSpec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Flat (VariantEncFlat, fromVariant, toVariant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type RemoteDataVarEnc =
  VariantEncFlat "kind"
    ( loading :: (progress :: Int)
    , success :: (result :: String)
    , failure ::
        ( error :: String
        , errCode :: Int
        )
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
    describe "toVariant" do
      it "should convert to and from Variant" do
        let
          v1 :: RemoteDataVar
          v1 = V.inj (Proxy :: _ "loading") { progress: 99 }

          v1' = fromVariant (Proxy :: _ "kind") v1

          v2 :: RemoteDataVar
          v2 = V.inj (Proxy :: _ "success") { result: "a" }

          v2' = fromVariant (Proxy :: _ "kind") v2

          v3 :: RemoteDataVar
          v3 = V.inj (Proxy :: _ "failure") { error: "", errCode: 0 }

          v3' = fromVariant (Proxy :: _ "kind") v3

        toVariant v1' `shouldEqual` v1
        toVariant v2' `shouldEqual` v2
        toVariant v3' `shouldEqual` v3

