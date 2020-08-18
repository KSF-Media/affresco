let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200615/packages.dhall sha256:5d0cfad9408c84db0a3fdcea2d708f9ed8f64297e164dc57a7cf6328706df93a
    with toppokki.repo = "https://github.com/f-f/purescript-toppokki.git"
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
          , repo =
              "https://github.com/KSF-Media/purescript-facebook.git"
          , version =
              "e765b3fb5585b2d774cc6a12c412891be51ca5ed"
          }
      , react-basic-router =
          { dependencies =
              [ "react-basic", "foreign-generic", "prelude" ]
          , repo =
              "https://github.com/KSF-Media/purescript-react-basic-router.git"
          , version =
              "03b5ce8462f57d930929dc6d037b1093fece2128"
          }
      , uuid =
          { dependencies =
              [ "effect", "maybe" ]
          , repo =
              "https://github.com/spicydonuts/purescript-uuid.git"
          , version =
              "572191b9f8053bc618c325289be9d6464865b12c"
          }
      , affresco-components =
          ./packages/components/spago.dhall as Location
      , affresco-user =
          ./packages/user/spago.dhall as Location
      , affresco-vetrina =
          ./packages/vetrina/spago.dhall as Location
      }

in  upstream // additions
