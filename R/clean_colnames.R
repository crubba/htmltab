#' Produces correct column names
#'
#' @param header.name.table the header column names vector
#' @param colNames either a self-specificed character vector for the column names or a function used on header.name.table
#' @param xpath generated header and body xpath
#' @return a character vector of header column names
clean_colnames <- function(header.name.table = NULL, colNames = NULL, xpath) {

  if(length(header.name.table) == 0 && is.null(colNames)){
    warning(sprintf("No header generated. Try passing information to header or colNames. Header XPath was %s", xpath[1]), call. = FALSE)
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
