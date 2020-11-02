
.onLoad <- function (libname, pkgname) {
    system2("docker", args=c("stop", "express-container"),
            stdout=FALSE, stderr=FALSE, wait=FALSE)
    options(layoutEngine.backend=ExpressEngine,
            layoutEngine.WSSession=NULL,
            layoutEngine.wsID=NULL,
            layoutEngine.wsMessage=NULL)
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to the Express server backend for the layout engine.")
}
