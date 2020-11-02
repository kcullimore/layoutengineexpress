

WSSessionOpen <- function(WS) {
    WSS_ready <- FALSE    
    WS_ready <- WS$getStatus()$ready
    if (!WS_ready) {
        WS$open()
    }
##    browser_session <- getOption("layoutEngine.wsID")
##    browser_count <- length(browser_session)
##    if (browser_count == 0) {
##        WS$open()
##        ID <- getOption("layoutEngine.wsID")
##        browser_session <- getOption("layoutEngine.wsID")
##        WSS_ready <- TRUE        
##        message("Express browser session has been created.")
##    } else {
##        WSS_ready <- TRUE
##    }
    WSS <- list(ready=TRUE,
                 ws=WS$ws,
                 dir=WS$dir,
                 close=WS$close,
                session=1
                )
    options(layoutEngine.WSSession=WSS)
    WSS
}

WSSessionClose <- function(WSS) {
    if (!WSS$ready) {
        message("Express browser session is not active so cannot be closed.")
    } else {
        WSS$close()  
    }
}

WSSession <- function(url="0.0.0.0", portServer=8080L, portClient="8080",
                       network="bridge", fresh_pull=FALSE) {

    settings <- list(url=url, portServer=portServer, portClient=portClient,
                     network=network, fresh_pull=fresh_pull)

    ## Express server setup
    WS <- webSocket(settings)

    ## RSelenium browser session setup 
    WSS <- WSSessionOpen(WS)
    
    open <- function() {
        WSSessionOpen(WS)
    }

    close <- function() {
        WSSessionClose(WSS)
    }
    c(WSS, open=open, close=close)
}
