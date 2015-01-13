#' Creates header using span information
#'
#' @param header.names a list of header names
#' @param header.colspans a list of header colspans
#' @param header.rowspans a list of header rowspans
#' @param headerSep a character vector that is used as a seperator in the construction of the table's variable names (default value ' >> ')
#' @return a vector of header column names

#Create header with spans
span_header <- function(header.names, header.colspans, header.rowspans, headerSep) {

  #has no header information
  if(length(header.names) == 0){
    header.name.table <- vector()
    return(header.name.table)
  }

  #Remove rows which have all empty cells
  empty.rows <- which(sapply(header.names, function(x) all(x == "")))
  if(!is_empty(empty.rows)){
    header.names <- header.names[-empty.rows]
    header.colspans <- header.colspans[-empty.rows]
    header.rowspans <- header.rowspans[-empty.rows]
  }

  #return empty header
  if(length(header.names) == 0){
    header.name.table <- vector()
    return(header.name.table)
  }

  header.name.table <- expand_header(header.names, header.colspans, header.rowspans)

  header.name.table <- lapply(header.name.table, function(col) col[!is.na(col)])

  header.name.table <- unlist(lapply(header.name.table, function(col) paste(col, collapse = headerSep)))

  return(header.name.table)
}
