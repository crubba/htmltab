#' Retrieve table head rows
#'
#' @param table.Node the table node
#' @param header a vector that contains information for the identification of the header row(s). A numeric vector can be specified where each element corresponds to the table rows. A character vector may be specified that describes an XPath for the header rows. If left unspecified, htmltable tries to use semantic information from the HTML code
#' @return the header element

get_head <- function(table.Node, header) {

  head <- XML::xpathSApply(table.Node, path = header)

  return(head)
}
