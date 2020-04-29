{ name = "my-project"
, dependencies = [ "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "pages/**/*.purs","src/**/*.purs", "test/**/*.purs" ]
}
