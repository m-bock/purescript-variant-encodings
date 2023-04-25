module Test.Data.Variant.Encodings.FlatSpec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Flat (VariantEncFlat, variantFromVariantEnc, variantToVariantEnc)
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
    describe "variantFromVariantEnc" do
      it "should convert to and from Variant" do
        let
          v1 :: RemoteDataVar
          v1 = V.inj (Proxy :: _ "loading") { progress: 99 }

          v1' = variantToVariantEnc v1 :: VariantEncFlat "kind" _

          v2 :: RemoteDataVar
          v2 = V.inj (Proxy :: _ "success") { result: "a" }

          v2' = variantToVariantEnc v2 :: VariantEncFlat "kind" _

          v3 :: RemoteDataVar
          v3 = V.inj (Proxy :: _ "failure") { error: "", errCode: 0 }

          v3' = variantToVariantEnc v3 :: VariantEncFlat "kind" _

        variantFromVariantEnc v1' `shouldEqual` v1
        variantFromVariantEnc v2' `shouldEqual` v2
        variantFromVariantEnc v3' `shouldEqual` v3

