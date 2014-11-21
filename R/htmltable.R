
#' Assemble information from HTML tables
#'
#' @param doc the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process.
#' @param which a vector identifying which tables to return from within the document (when such was specified). Either a numeric vector for the tables' rank or a character vector specifiying an XPath for the tables.
#' @param header a vector specifying header information. A numeric vector can be specified where each element corresponds to the table rows. A character vector may be specified that describes an XPath for the header rows.
#' @param headerSep a character vector that is used for constructing R table variable names from the HTML table.
#' @param body a vector specifying body information. A numeric vector can be specified where each element corresponds to the table rows. A character vector may be specified that describes an XPath for the body rows.
#' @param cellFun a list of functions that is executed over the header and body cell nodes.
#' @param colClasses a vector of classes for the columns.
#' @param ... additional arguments
#' @return A list of R data frames (if as.data.frame is left at its default value).
#' @export
#' @examples
#'
#' This table lacks header information
#' We pass NULL to skip the header generation step
#' url <- "http://apps.who.int/immunization_monitoring/globalsummary/timeseries/tswucoveragebcg.html"
#' htmltable(doc = url, which = "/html/body/div/table[3]", header = NULL)
#'
#'
#' This example illustrates extracting multiple tables
#' url <-
htmltable <- function(doc,
                      which = NULL,
                      header = NULL,
                      headerSep = " >> ",
                      body = NULL,
                      elFun = function(node)XML::xmlValue(node),
                      colClasses = NULL,
                      as.data.frame = TRUE, ...){

  args <- list(...)

  # Check Inputs ---------------------------
  Node <- check_type(doc, which)

  tab.list <- list()
  for(i in 1:length(Node)) {

    table.Node <- Node[[i]]

  # Create Header ---------------------------

    #Retrieve Head Elements
  head <- get_head(table.Node = table.Node, header = header)

  # Header Position
  #header.position <- get_header_position(header = header, header.)

  header.colspans <- get_colspans(head, tag = "td | th") #colspans for relevant row values
  header.rowspans <- get_rowspans(head, tag = "td | th") #rowspans so far ignored
  header.names <- get_cell_element(head, tag = "td | th", elFun = function(node)XML::xmlValue(node))

  #Span head
  header.name.table <- span_header(header.names, header.colspans, header.rowspans, headerSep = headerSep)


  # Create Body ---------------------------

  #Get Body Cell Nodes
  cells <- get_cells(table.Node = table.Node, body = body) #change header to header.location

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

  tab.list[[i]] <- tab

  }

  #Return df if only single tab
  if(length(tab.list) == 1) tab.list <- tab.list[[1]]

  return(tab.list)
}
