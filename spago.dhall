{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "enums"
  , "halogen"
  , "halogen-formless"
  , "maybe"
  , "now"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
