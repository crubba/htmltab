#' Produces correct column names
#'
#' @param header.name.table the table node
#' @param colNames either a self-specificed character vector for the column names or a function used on header.name.table
#' @return a character vector

clean_colnames <- function(header.name.table = NULL, colNames = NULL) {

  if(is.null(colNames)) {
    tab.names <- header.name.table
  }

  if(is.character(colNames)) {
    tab.names <- colNames
  }

  if(class(colNames) == "function") {
    tab.names <- colNames(header.name.table)
  }

  return(tab.names)

}
