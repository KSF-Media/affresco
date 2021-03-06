let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210331/packages.dhall sha256:fe3b63fe4b0cd1518c0ee506751b5b16d2c47210df94b5beb48be6570fe7f78a
      with toppokki.repo
	   = "https://github.com/KSF-Media/purescript-toppokki.git"
      with toppokki.version = "81e63299d0765e4b54cb0e26a5edc9d7b6cc5036"

let additions =
      { facebook =
	{ dependencies =
	  [ "console"
	  , "aff"
	  , "prelude"
	  , "foreign"
	  , "foreign-generic"
	  , "errors"
	  , "effect"
	  ]
	, repo = "https://github.com/KSF-Media/purescript-facebook.git"
	, version = "f3fb7727fa5d0ddc9f102e6970df669a9d3e3c36"
	}
      , payload =
	{ dependencies =
	  [ "aff"
	    , "affjax"
	    , "console"
	    , "debug"
	    , "effect"
	    , "foreign-generic"
	    , "node-fs"
	    , "node-fs-aff"
	    , "node-http"
	    , "prelude"
	    , "psci-support"
	    , "record"
	    , "simple-json"
	    , "stringutils"
	    , "test-unit"
	    , "typelevel-prelude"
	  ]
	, repo = "https://github.com/hoodunit/purescript-payload.git"
	, version = "f1f707490c5463029765c15a960d799c9972be56"
      }
      , simple-ajax =
	{ repo = "https://github.com/dariooddenino/purescript-simple-ajax.git"
	      , version = "a8c6925db0463385a465a269c894f770b405460c"
	      , dependencies = [ "affjax", "simple-json", "variant" ]
	      }
      , react-basic-router =
	{ dependencies = [ "react-basic", "foreign-generic", "prelude" ]
	, repo =
	    "https://github.com/KSF-Media/purescript-react-basic-router.git"
	, version = "65ae7ae9dfd89213d11ce6679594ccf4dea8576e"
	}
      , uuid =
	{ dependencies = [ "effect", "maybe" ]
	, repo = "https://github.com/spicydonuts/purescript-uuid.git"
	, version = "572191b9f8053bc618c325289be9d6464865b12c"
	}
      , web-url =
        { dependencies = [ "effect" ]
	, repo =
	    "https://github.com/mjepronk/purescript-web-url.git"
        , version = "d654e3b550473874461b61013854350b4d944c11"
	}
      , affresco-components = ./packages/components/spago.dhall as Location
      , affresco-test = ./packages/test/spago.dhall as Location
      , affresco-user = ./packages/user/spago.dhall as Location
      , affresco-vetrina = ./packages/vetrina/spago.dhall as Location
      }

in  upstream // additions
