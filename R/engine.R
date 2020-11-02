
## CSS standard says 1px = 1/96in !?
dpi <- 96

## Primary function to generate layout within the RSelenium hosted
## browser and return to R
ExpressLayout <- function(html, width, height, fonts, device) {
    ## Initiate a live browser session
    global_WSS <- getOption("layoutEngine.WSSession")
    if (is.null(global_WSS)) {
        WSS <- WSSession()
    } else {
        WSS <- global_WSS
    }
    
    ## Define remote driver pointer
    ws <- WSS$ws
    ## Define working directory
    wd <- WSS$dir
    asset_dir <- file.path(wd, "assets")
    ## Copy font files
    fontFiles <- fontFiles(fonts, device)
    file.copy(fontFiles, asset_dir)
    ## Convert any .pfb/.pfa to .ttf
    pffiles <- grepl("[.]pf[ab]$", fontFiles)
    if (any(pffiles)) {
        fontforge <- Sys.which("fontforge")
        if (nchar(fontforge) == 0) stop("FontForge not available")
        for (i in fontFiles[pffiles]) {
            system2(fontforge,
                    args=c("-quiet", "-lang=ff", "-script",
                           system.file("FF", "pf2ttf",
                                       package="layoutEngineRSelenium"),
                           " ", file.path(asset_dir, basename(i))
                           ), stderr=FALSE)
        }}
    ## Copy all assets to asset directory
    copyAssets(html, asset_dir)

    ## Set the page <body> size to match R graphics device
    body <- xml_find_first(html$doc, "body")    
    xml_set_attr(body,
                 "style",
                 paste0("width: ", as.character(width*dpi), "px; ",
                        "height: ", as.character(height*dpi), "px;"))

    ## divide components for transport via WebSocket
    new_body <- paste(xml_find_first(html$doc, "body"), collapse="")
    new_head <- paste(xml_find_first(html$doc, "head"), collapse="")
    data <- as.character(jsonlite::toJSON(list(head=new_head, body=new_body)))
    browser()
    ## Send HTML to browser via WebSocket connection
    ws$send(data)

    ## Get layout
    layoutCSV <- getOption("layoutEngine.wsMessage")
 
    ## Build data.frame with layout data
    layoutDF <- read.csv(text = layoutCSV,
                         header=FALSE, stringsAsFactors=FALSE,
                         quote="'\"")
    names(layoutDF) <- names(layoutFields)
    ## Convert font size from CSS pixels to points
    layoutDF$size <- layoutDF$size * 72 / dpi
    do.call(makeLayout, layoutDF)
}

ExpressfontFile <- function(file) {
    ## Strictly, @font-face spec does not allow .pfb/.pfa
    ## Replace .pfb/.pfa with .ttf
    ## (the conversion of the actual font happens in the layout function)
    gsub("[.]pf[ab]$", ".ttf", file)
}

ExpressEngine <- makeEngine(ExpressLayout,
                            cssTransform=list(
                                fontFile=ExpressfontFile))
