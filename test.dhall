{ name = "variant-encodings-test"
, dependencies =
  (./spago.dhall).dependencies # 
  [ "console"
  , "effect"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "aff"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
