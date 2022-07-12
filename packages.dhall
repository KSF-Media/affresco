let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220613/packages.dhall
        sha256:99f976d547980055179de2245e428f00212e36acd55d74144eab8ad8bf8570d8

let additions =
      { facebook =
        { dependencies =
          [ "console"
          , "aff"
          , "prelude"
          , "foreign"
          , "foreign-generic"
          , "effect"
          ]
        , repo = "https://github.com/KSF-Media/purescript-facebook.git"
        , version = "54bd9ecf552f686bf8edd603dcdb16498350cb9f"
        }
      , foreign-generic =
        { repo = "https://github.com/jsparkes/purescript-foreign-generic"
        , version = "844f2ababa2c7a0482bf871e1e6bf970b7e51313"
        , dependencies = [ "arrays", "assert", "bifunctors", "console", "control", "effect", "either", "exceptions", "foldable-traversable", "foreign", "foreign-object", "identity", "lists", "maybe", "newtype", "partial", "prelude", "record", "strings", "transformers", "tuples", "typelevel-prelude", "unsafe-coerce"]
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
        , repo = "https://github.com/KSF-Media/purescript-payload.git"
        , version = "dd6132b2243dd89bd04eb149d7ee2c403d4753e0"
        }
      , react-basic-classic =
        { repo = "https://github.com/lumihq/purescript-react-basic-classic"
        , version = "499c6e481c5947eb231ce7d4076afba9c0908b37"
        , dependencies = [ "aff", "effect", "functions", "maybe", "nullable", "prelude", "react-basic"]
        }
      , react-basic-hooks =
        { repo = "https://github.com/megamaddu/purescript-react-basic-hooks"
        , version = "e7494bd4656b4a43c2efc69bf5f512e154f05cc1"
        , dependencies = [ "aff", "aff-promise", "bifunctors", "console", "control", "datetime", "effect", "either", "exceptions", "foldable-traversable", "functions", "indexed-monad", "integers", "maybe", "newtype", "now", "nullable", "ordered-collections", "prelude", "react-basic", "refs", "tuples", "type-equality", "unsafe-coerce", "unsafe-reference", "web-html"]
        }
      , react-basic-dom =
        { repo = "https://github.com/lumihq/purescript-react-basic-dom"
        , version = "53dfc605a1dd91dbc12df160b445156e6a6626af"
        , dependencies = [ "effect", "foldable-traversable", "foreign-object", "maybe", "nullable", "prelude", "react-basic", "unsafe-coerce", "web-dom", "web-events", "web-file", "web-html"]
        }
      , simple-json =
        { repo = "https://github.com/justinwoo/purescript-simple-json"
        , version = "b85e112131240ff95b5c26e9abb8e2fa6db3c656"
        , dependencies = [ "prelude", "typelevel-prelude", "record", "variant", "nullable", "foreign-object", "foreign", "exceptions", "arrays" ]
        }
      , toppokki =
        { repo = "https://github.com/KSF-Media/purescript-toppokki"
	, version = "8d2084ce1ef5019b60935b21c4f8558fcea1a166"
	, dependencies = [ "aff-promise", "console", "effect", "functions", "milkis", "node-buffer", "node-fs-aff", "node-http", "node-process", "prelude", "psci-support", "record", "test-unit" ]
        }
      , milkis =
        { repo = "https://github.com/justinwoo/purescript-milkis"
        , version = "892b25d0186aa0601c657bc3b30a36a91b92aa7e"
        , dependencies = [ "prelude", "aff-promise", "typelevel-prelude", "foreign-object", "arraybuffer-types" ]
        }
      , unordered-collections =
        { repo = "https://github.com/fehrenbach/purescript-unordered-collections"
        , version = "6fb203af23c9910ca6776d0673a0256690e0fbcd"
        , dependencies = [ "arrays", "enums", "functions", "integers", "lists", "prelude", "record", "tuples", "typelevel-prelude", "unfoldable" ]
        }
      , uuid =
        { dependencies = [ "effect", "maybe" ]
        , repo = "https://github.com/megamaddu/purescript-uuid.git"
        , version = "7bb5a90c9b11f6a33ac7610608a650e4d58aeac9"
        }
      , affresco-components = ./packages/components/spago.dhall as Location
      , affresco-test = ./packages/test/spago.dhall as Location
      , affresco-user = ./packages/user/spago.dhall as Location
      , affresco-vetrina = ./packages/vetrina/spago.dhall as Location
      }

in  upstream // additions
