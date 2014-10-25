
#' Assemble information from HTML tables.
#'
#' @param Node A or character.
#' @param which Either numeric or character.
#' @param header numeric or character.
#' @param headerSep a vector
#' @param body numeric or character
#' @param cellFun A function on the node
#' @param colClasses a vector of classes
#' @param ... additional arguments
#' @return A list The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
htmltable <- function(doc,
                      num = NULL, #which
                      header = NULL,
                      headerSep = c(" >1> ", " >2> ", " >3> ", " >4> "),
                      body = NULL,
                      elFun = function(node)XML::xmlValue(node),
                      colClasses = NULL,
                      as.data.frame = TRUE, ...){ #followFootnotes

  library(magrittr)
  library(XML)

  # Check Inputs ---------------------------
  Node <- check_type(doc, num)


  # Create Header ---------------------------

  #Retrieve Head Elements
  head <- get_head(Node, header = header)

  # Header Position
  #header.position <- get_header_position(header = header, header.)

  header.colspans <- get_colspans(head, tag = "td | th") #colspans for relevant row values
  header.rowspans <- get_rowspans(head, tag = "td | th") #rowspans so far ignored
  header.names <- get_cell_element(head, tag = "td | th", elFun = function(node)XML::xmlValue(node))

  #Span head
  header.name.table <- span_header(header.names, header.colspans, header.rowspans, headerSep = headerSep)


  # Create Body ---------------------------

  #Get Body Cell Nodes
  cells <- get_cells(Node, body = body) #change header to header.location

  #Extract and transform body cell elements
  vals <- get_cell_element(cells, elFun = elFun)

  #Produce rowspans and colspans lists from body cell
  body.rowspans <- get_rowspans(cells)
  body.colspans <- get_colspans(cells)

  #Produce table body
  tab <- span_body(vals, colspans = body.colspans, rowspans = body.rowspans) #used to be "span.table"


  # Finish ---------------------------

  #Give Column Names
  colnames(tab) <- header.name.table

  tab <- as.data.frame(tab, stringsAsFactors = F)


  return(tab)
}
