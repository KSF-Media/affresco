{-
    Container image used in builds
-}
let container = { image = "ksfmedia/diskho:gha-0.1", options = "--cpus 2" }

in  container
