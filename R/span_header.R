
#Create header with spans
span_header <- function(header.names, header.colspans, header.rowspans, headerSep) {

  #has no header information
  if(length(header.names) == 0){
    header.name.table <- vector()
    return(header.name.table)
  }

  header.name.table <- expand_header(header.names, header.colspans, header.rowspans)

  header.name.table <- lapply(header.name.table, function(col) col[!is.na(col)])

  header.name.table <- unlist(lapply(header.name.table, function(col) paste(col, collapse = headerSep)))

  return(header.name.table)
}
