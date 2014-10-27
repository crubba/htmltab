
#' Assemble Check input values
#'
#' @param doc the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process.
#' @param which a vector identifying which tables to return from within the document (when such was specified). Either a numeric vector for the tables' rank or a character vector specifiying an XPath for the tables.
#' @return A list of table nodes.

check_type <- function(doc, which){

  #URL input
  if(is.character(doc)) {
#    if(!grepl("^www|^http", doc)) stop("Invalid 'doc' input: String is no URL") #doesnt work for files saved on hard drive
    Node <- XML::htmlParse(doc)
    } else if(class(doc) == "HTMLInternalDocument") {
      Node <- doc
    }

  #Check if table xpath specified
  if(!is.null(which)) {
    if(is.character(which)) {
      Node <- XML::getNodeSet(Node, path = which)
    }
    if(is.numeric(which)){
      Node <- XML::getNodeSet(Node, path = sprintf("//table[%i]", which))
    }
  }

  return(Node)
}
