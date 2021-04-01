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
      , affresco-components = ./packages/components/spago.dhall as Location
      , affresco-user = ./packages/user/spago.dhall as Location
      , affresco-vetrina = ./packages/vetrina/spago.dhall as Location
      }

in  upstream // additions
