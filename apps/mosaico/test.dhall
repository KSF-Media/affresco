let conf  =./spago.dhall

in conf // {
    sources = conf.sources # [ "test/**/*.purs" ],
    dependencies = conf.dependencies #
        [ "affresco-test"
        , "debug"
        , "partial"
	, "enums"
	    , "test-unit"
	    ]
}
