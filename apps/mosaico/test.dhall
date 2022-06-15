let conf  =./spago.dhall

in conf // {
    sources = conf.sources # [ "test/**/*.purs" ],
    dependencies = conf.dependencies #
        [ "affresco-test"
        , "partial"
	, "enums"
        , "transformers"
	    , "test-unit"
	    ]
}
