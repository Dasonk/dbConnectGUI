#' Get a connection to a MySQL database
#' 
#' Provides a GUI to get a connection to a MySQL
#' database.
#' 
#' @export
#' @examples
#' \dontrun{
#' conn <- getConnection()
#' dbConnectGUI(conn)
#' }
getConnection <- function(){
    options("guiToolkit" = "RGtk2")
    start <- Sys.time()
    win <- gwindow("Connection Dialog", width = 300, height = 100)
    gg <- ggroup(horizontal = FALSE, cont = win)

    serverFrame <- gframe("Server Host:", cont = gg)
    serverEdit <- gedit(text = "headnode.stat.iastate.edu",
                        editable = TRUE,
                        cont = serverFrame,
                        expand = TRUE)

    portFrame <- gframe("Port:", cont = gg)
    portEdit <- gedit(text = "3306",
                      editable = TRUE,
                      cont = portFrame,
                      expand = TRUE)

    databaseFrame <- gframe("Database", cont = gg)
    databaseEdit <- gedit("data_expo_2009",
                          editable = TRUE,
                          cont = databaseFrame,
                          expand = TRUE)

    userFrame <- gframe("User:", cont = gg)
    userEdit <- gedit(text = "2009Expo",
                      editable = TRUE,
                      cont = userFrame,
                      expand = TRUE)

    passwordFrame <- gframe("Password", cont = gg)
    passwordEdit <- gedit(text = "",
                          editable = TRUE,
                          cont = passwordFrame,
                          expand = TRUE)
    visible(passwordEdit) <- FALSE
    svalue(passwordEdit) <- "R R0cks"

    buttonGroup <- ggroup(container = gg, horizontal = TRUE)
    connectButton <- gbutton("Connect", cont = buttonGroup)
    cancelButton <- gbutton("Cancel", cont = buttonGroup)

    drv <- dbDriver("MySQL")
    con <- NULL
    status <- "Not Connected"
    errormessage <- NULL
    continue <- FALSE

    errorFunction <- function(e){
        errormessage <<- e
        status <<- "Not Connected"

        tmp <- tail(strsplit(e$message, "Error: ")[[1]], 1)
        gmessage(paste("Failed to connect to database\nReason:",
                       tmp),
                 icon = "error",
                 parent = win)

        return(NULL)
    }

    connectHandler <- function(h, ...){
        tmp <- suppressWarnings(as.numeric(svalue(portEdit)))
        if(is.na(tmp)){
            gmessage("Port must be numeric!",
                     icon = "warning",
                     parent = win)
            return()
        }

        status <<- "Connected"
        con <<- tryCatch(dbConnect(drv,
                                   user = svalue(userEdit),
                                   password = svalue(passwordEdit),
                                   port = as.numeric(svalue(portEdit)),
                                   dbname = svalue(databaseEdit),
                                   host = svalue(serverEdit)),
                         error = errorFunction)

        svalue(connectButton) <- "Connected"
        continue <<- TRUE
        dispose(win)
    }
    addHandlerClicked(connectButton, connectHandler)

    cancelHandler <- function(h, ...){
        continue <<- TRUE
        dispose(win)
    }
    addHandlerClicked(cancelButton, cancelHandler)

    while(!continue){
        Sys.sleep(.2)
        ## if(as.numeric(Sys.time() - start) > 3*60){
        ##     errormessage <<- "You took too long!"
        ## }
    }

    return(list(connection = con, 
                driver = drv, 
                status = status, 
                error = errormessage))

}


