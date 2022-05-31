{-
    Container image used in builds
-}
let container = { image = "ksfmedia/diskho:gha-1.2", options = "--cpus 2 --user root" }

in  container
