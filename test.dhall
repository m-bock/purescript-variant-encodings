{ name = "variant-encodings-test"
, dependencies =
  [ "aff"
  , "effect"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
