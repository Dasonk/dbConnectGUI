#' Convert data types from SQL to R
#'
#' Takes a vector of SQL data types and and returns the corresponding
#' data types in R.
#'
#' @param data The vector of SQL data types to convert
#' @return A vector of R data types.
sqlToR <- function(data){
    data[data %in% c("varchar", "char", "date", "text")] <- "character"
    data[data %in% c("int", "decimal", "bigint", "double")] <- "numeric"
    return(data)
}