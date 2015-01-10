#' Assemble Check input values
#'
#' @param table.Node the table node
#' @param header an information on the header rows
#' @return A list of header information

get_head <- function(table.Node, header) {

  head <- XML::xpathSApply(table.Node, path = header)

  return(head)
}
