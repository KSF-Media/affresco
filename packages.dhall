let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201021/packages.dhall sha256:55ebdbda1bd6ede4d5307fbc1ef19988c80271b4225d833c8d6fb9b6fb1aa6d8
      with toppokki.repo
           = "https://github.com/KSF-Media/purescript-toppokki.git"
      with toppokki.version = "81e63299d0765e4b54cb0e26a5edc9d7b6cc5036"
      with react-basic-router = /home/kaol/src/purescript-react-basic-router/spago.dhall as Location

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
        , version = "e765b3fb5585b2d774cc6a12c412891be51ca5ed"
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
