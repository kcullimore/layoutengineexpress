
wsStatus <- function(WS) {
    container_info <- WS$container$getInfo()
    if (!container_info$running) {
        ready <- FALSE
    } else {
        ready <- (WS$ws$readyState() == 1)
    }
    list(ready=ready)
}

wsOpen <- function(settings) {
    ## Docker container setup and status variables
    container <- dockerContainer(settings)
    container$run()
    container_info <- container$getInfo()

    ## Establish websocket connection
    ws <- websocket::WebSocket$new(paste0("ws://",
                                          settings$url, ":",
                                          settings$portServer),
                                   autoConnect=FALSE,
                                   maxMessageSize=1e6)
    
    ws$onOpen(function(){
        message("Websocket connection opened.")
    })
    ## Define onMessage behavior
    ws$onMessage(function(event) {
        msg <- invisible(jsonlite::fromJSON(event$data))
        ID <- getOption("layoutEngine.wsID")
        if (msg$event == "receipt") {            
            options(layoutEngine.wsID=msg$id)    
        } else if (msg$event == "message") {
            options(layoutEngine.wsMessage=msg$message)            
        }
    })

    ws$onClose(function() {
        message("Websocket connection to Express server closed.")
    })

    ws$connect()
    
    list(ws=ws, container=container, dir=container_info$dir)
}

wsClose <- function(WS) {
    container_info <- WS$container$getInfo()
    name <- WS$container$name
    if (!container_info$running) {
        message("Express server is not running.")
    } else {
        options(layoutEngine.WSSession=NULL)
        options(layoutEngine.wsID=NULL)
        options(layoutEngine.wsMessage=NULL)
        WS$ws$close()
        WS$container$close()
        message(paste0("Express server running in docker container '",
                       name, "' has been closed."))
    }
}

webSocket <- function(settings) {

    WS <- wsOpen(settings)
    getStatus <- function() {
        wsStatus(WS)
    }

    open <- function() {
        wsOpen(settings)
    }

    close <- function() {
        wsClose(WS)
    }

    ## rSServer object
    c(WS, getStatus=getStatus, open=open, close=close)
}
