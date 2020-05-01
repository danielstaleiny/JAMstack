{ name = "my-project"
, dependencies =
  [ "console", "effect", "lists", "maybe", "psci-support", "web-html" ]
, packages = ./packages.dhall
, sources = [ "pages/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
