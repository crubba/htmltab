
#' Assemble Check input values
#'
#' @param doc the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process.
#' @param which a vector identifying which tables to return from within the document (when such was specified). Either a numeric vector for the tables' rank or a character vector specifiying an XPath for the tables.
#' @return A list of table nodes.

check_type <- function(doc, which, ...){

  #XML input
  if(any(class(doc) == "HTMLInternalDocument")) {
    Node <- XML::getNodeSet(doc, "//table") #returns all tables
    }

  #doc is list of parsed table nodes
  if(is.list(doc)) {
    Node <- doc
  }

  #URL input
  if(is.character(doc)) {
    #if(!("encoding" %in% names(args))) encoding <- read_charset(doc)
    parsed_doc <- XML::htmlParse(doc, ...)

    if (is.character(which)) {
      xpath <- paste(which, collapse = " | ")
      Node <- XML::getNodeSet(parsed_doc, path = xpath) #needs to work for vector which input
    }

    if (is.numeric(which)) {
      xpath <-  sprintf("//table[%i]", which)
      xpath <- paste(xpath, collapse = " | ")
      Node <- XML::getNodeSet(parsed_doc, path = xpath) #needs to work for vector which input
    }

    if (!is.numeric(which) & !is.character(which)) {
      Node <- XML::getNodeSet(parsed_doc, path = "//table")
    }
  }

  if(!exists("Node")) stop("doc is of unknown type")

  return(Node)
}
