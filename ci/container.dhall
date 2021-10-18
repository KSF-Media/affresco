{-
    Container image used in builds
-}
let container = { image = "ksfmedia/diskho:gha-1.0", options = "--cpus 2" }

in  container
