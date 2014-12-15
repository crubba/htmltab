#' Produces correct column names
#'
#' @param header.name.table the table node
#' @param colNames either a self-specificed character vector for the column names or a function used on header.name.table
#' @return a character vector

clean_colnames <- function(header.name.table = NULL, colNames = NULL) {

  if(length(header.name.table) == 0 && is.null(colNames)){
    warning("No header generated. Try passing information to header or colNames.")
    return(NULL)
    }


  if(length(header.name.table) > 0 && is.null(colNames)) {
    tab.names <- header.name.table
    return(tab.names)
  }

  if(is.character(colNames)) {
    tab.names <- colNames
    return(tab.names)
  }

  if(class(colNames) == "function") {
    tab.names <- colNames(header.name.table)
  }

  return(tab.names)

}
