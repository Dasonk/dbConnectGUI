
#' Convert data types from SQL to R
#'
#' Takes a vector of SQL data types and and returns the corresponding
#' data types in R.
#'
#' @param data The vector of SQL data types to convert
#' @return A vector of R data types.
#' @author Dason Kurkiewicz \email{dasonk@@iastate.edu}
sqlToR <- function(data){
    data[data %in% c("varchar", "char", "date", "text")] <- "character"
    data[data %in% c("int", "decimal", "bigint", "double")] <- "numeric"
    return(data)
}

dbConnector <- function(connection = NULL){

    ## Rename to j for ease of use
    con <- connection

    ## If nothing was supplied then get a connection
    if(is.null(con)){
        j <- getConnection()

        if(j$status == "Not Connected"){
            return(j)
        }

        con <- j$connection
    }

    dbname <- dbGetInfo(con)$dbname

    ## Main Window
    win <- gwindow(dbname)
    gg <- ggroup(hor = F, cont = win, expand = T)
    ## Main notebook
    nb <- gnotebook(cont = gg, expand = T)

    destroyHandler <- function(h, ...){
        dbDisconnect(con)
    }

    addHandlerDestroy(win, handler = destroyHandler)

    tables = list()
    tables$names <- dbListTables(con)
    tables$count <- integer(length(tables$names))

    fields <- list()
    for(i in seq_along(tables$names)){
        fields[[i]] <- dbListFields(con, tables$names[i])
        tmp <- dbGetQuery(con, paste("SELECT COUNT(*) FROM",
                                     tables$names[i]))
        tables$count[i] <- as.numeric(tmp)
    }

    dvGroup <- ggroup(hor = F, cont = nb, expand = T,
                      label = "Data Viewer")
    vbGroup <- ggroup(hor = F, cont = nb, expand = T,
                      label = "Variable Browser")
    ssGroup <- ggroup(hor = F, cont = nb, expand = T,
                      label = "Subsample")
    queryGroup <- ggroup(hor = F, cont = nb, expand = T,
                         label = "Query")

    ## Set the notebook to the first tab
    svalue(nb) <- 1

    #################
    ## DATA VIEWER ##
    #################

    dvNB <- gnotebook(expand = TRUE)
    dvNBList <- list()

    ## Preprocess the tables by creating
    ## a tab in the notebook
    for(i in seq_along(tables$names)){
        if(tables$count[i] !=0){
            nm <- tables$names[i]
            dvNBList[[nm]] <- list()
            dvNBList[[nm]]$group <- ggroup(hor = F,
                                           cont = dvNB,
                                           label = nm,
                                           expand = T)
            dvNBList[[nm]]$activated <- FALSE
        }else{
            ## If there is an empty table in the database
            ## give a warning.
            warnMessage <- paste("Empty Table for:", tables$names[i])
            warning(warnMessage,
                    immediate. = TRUE,
                    call. = FALSE)
        }
    }

    ## This handler gets called when the tab is changed
    dvHandler <- function(h, ...){
        nm <- names(dvNBList)[h$pageno]

        ## If this tab isn't active yet then create it
        if(!dvNBList[[nm]]$activated){
            query <- paste("SELECT * FROM", nm, "LIMIT 5")
            table <- dbGetQuery(con, query)
            dvNBList[[nm]]$table <<- gdf(table)
            add(dvNBList[[nm]]$group, dvNBList[[nm]]$table, expand = TRUE)
            dvNBList[[nm]]$activated <<- TRUE
        }
    }

    addHandlerChanged(dvNB, handler = dvHandler)

    ## If there is only one table then the svalue
    ## call below doesn't activate the handler
    ## so we manually activate it here
    if(length(dvNBList) == 1){
        dvHandler(list(pageno = 1))
    }

    ## Change to the first tab
    svalue(dvNB) <- 1

    ## Add the notebook to it's group
    add(dvGroup, dvNB, expand = TRUE)


    ######################
    ## Variable Browser ##
    ######################

    vbNB <- gnotebook(expand = TRUE)
    vbNBList <- list()

    ## Preprocess the tables by creating
    ## a tab in the notebook
    for(nm in names(dvNBList)){
        vbNBList[[nm]] <- list()
        vbNBList[[nm]]$group <- ggroup(hor = F, cont = vbNB,
                                       label = nm, expand = TRUE)
        vbNBList[[nm]]$activated <- FALSE
    }

    ## This handler gets called when the tab is changed
    vbHandler <- function(h, ...){
        nm <- names(vbNBList)[h$pageno]

        ## If this tab isn't active yet then create it
        if(!vbNBList[[nm]]$activated){
            query <- paste("SELECT COLUMN_NAME, DATA_TYPE ",
                           "FROM INFORMATION_SCHEMA.COLUMNS ",
                           "WHERE table_name = '", nm, "'",
                           sep = "")
            table <- dbGetQuery(con, query)
            table$R_DATA_TYPE = sqlToR(table[,"DATA_TYPE"])
            vbNBList[[nm]]$table <<- gtable(table)
            add(vbNBList[[nm]]$group, vbNBList[[nm]]$table, expand = TRUE)
            vbNBList[[nm]]$activated <<- TRUE
        }
    }

    addHandlerChanged(vbNB, handler = vbHandler)

    ## If there is only one table manually activate handler
    if(length(dvNBList) == 1){
        vbHandler(list(pageno = 1))
    }

    ## Change to the first tab to activate handler
    svalue(vbNB) <- 1
    add(vbGroup, vbNB, expand = TRUE)


    ################
    ## SubSample  ##
    ################

    ## Nothing Yet

    ################
    ## Query      ##
    ################

    ## Nothing Yet
}

## library(gWidgets)
## library(gWidgetsRGtk2)
## gwindow()
## source("getConnection.R")
## j <- getConnection()
##
## dbConnector(j$connection)
