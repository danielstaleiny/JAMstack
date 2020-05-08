{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "console"
  , "debug"
  , "effect"
  , "functors"
  , "lists"
  , "maybe"
  , "psci-support"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "pages/assets/js/pure/**/*.purs", "test/**/*.purs" ]
}
