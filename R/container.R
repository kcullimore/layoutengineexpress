createTmpDir <- function() {
    ## Work in temp directory
    wd <- file.path(tempdir(), "layoutEngineExpress")
    if (!dir.exists(wd)) {
        result <- dir.create(wd, showWarnings=TRUE)
        if (!result) stop("Creation of working directory failed")
    }
    ## Create directory for assets
    asset_dir <- file.path(wd, "assets")
    if (!dir.exists(asset_dir)) {
        result <- dir.create(asset_dir, showWarnings=TRUE)    
        if (!result) stop("Creation of working directory failed")
    }
    wd
}


containerInfo <- function(name) {
    id <- system2("docker",
                  args=c("ps",  "--filter", paste0("'name=", name, "'"),
                         "--format", "'{{.ID}}'"),
                  stdout=TRUE, stderr=FALSE)
    running <- length(id) > 0
    if (running) {
        image <- system2("docker",
                         args=c("ps",  "--filter", paste0("'name=", name, "'"),
                                "--format", "'{{.Image}}'"),
                         stdout=TRUE, stderr=FALSE)
        mounts <- system2("docker",
                          args=c("inspect",  "-f", "'{{.Mounts}}'", id),
                          stdout=TRUE, stderr=FALSE)
        tmp <- strsplit(mounts, " ")
        dir <- tmp[[1]][grep("layoutEngineExpress", tmp[[1]])]
    } else {
        id <- image <- dir <- NULL
    }
    list(id=id, running=running, image=image, dir=dir)
}

containerRun <- function(name, settings) {
    ## Get info of container if running
    info <- containerInfo(name)
    ## Container image to build from
    image <- "kcull/layoutengine-express:latest"
    ## Container build/rebuild logic
    if (info$running && !settings$fresh_pull) {
        message(paste0("Docker container '", name,
                       "' is already running."))
    } else {
        if (info$running) containerClose(name)
        if (settings$fresh_pull) {
            system2("docker", args=c("pull", image))
            message(paste0("Docker container '", name,
                           "' is being rebuilt with freshly pulled image ",
                           image))
        }
        ## Create tmp directory for docker instance
        dir <- createTmpDir()
        run_args <- c("run",  "-d",  "--rm ", "--name", name,
                       "--volume", paste0(dir, ":/tmp/src"),
                      "--network", settings$network,
                      "-p ", paste0(settings$portServer, ":", settings$portClient),
                      image)
        system2("docker", run_args, stdout=TRUE, stderr=FALSE)
        Sys.sleep(2)
        info <- containerInfo(name)
        message(paste0("Docker container created with name=",
                       name, " and id=", info$id))
    }
}

containerClose <- function(name="express-container") {
    info <- containerInfo(name)
    if (info$running) {
        closed <- system2("docker", args=c("stop", name), stdout=TRUE, stderr=FALSE)
        message(paste0("Docker container '", closed, "' stopped and removed."))
        options(layoutEngine.rSSSession=NULL)         
    } else {
        message(paste0("Docker container '", name, "' is not running."))
    }
}

dockerContainer <- function(settings) {
    ## Container name
    name <- "express-container"

    getInfo <- function() {
        containerInfo(name)
    }

    run <- function () {
        containerRun(name, settings)
    }

    close <- function () {
        containerClose(name)
    }

    list(name=name, getInfo=getInfo, run=run, close=close)
}
