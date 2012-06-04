
if(!exists("first")){
    first <- FALSE
    library(gWidgetsRGtk2)
    library(RMySQL)
    source("getConnection.R")
    j <- getConnection()
}

if(FALSE){
    server <- "headnode.stat.iastate.edu"
    port <- 3306
    database <- "data_expo_2009"
    user <- "2009Expo"
    pass <- "R R0cks"
    
    drv <- dbDriver("MySQL")
    con <- dbConnect(drv, user=user, password=pass, port=port, host=server)
    dbGetQuery(con, "SHOW DATABASES")
}

source("dbConnectGUI.R")

dbConnectGUI(j$con)



con <<- tryCatch(dbConnect(drv,
                           user = svalue(userEdit),
                           password = svalue(passwordEdit),
                           port = as.numeric(svalue(portEdit)),
                           dbname = svalue(databaseEdit),
                           host = svalue(serverEdit)),