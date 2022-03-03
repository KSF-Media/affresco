{-
    Container image used in builds
-}
let container = { image = "ksfmedia/diskho:gha-1.1", options = "--cpus 2" }

in  container
