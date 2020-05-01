{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "debug"
  , "effect"
  , "functors"
  , "lists"
  , "maybe"
  , "psci-support"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "pages/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
