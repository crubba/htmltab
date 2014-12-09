
#' Assemble Check input values
#'
#' @param doc the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process.
#' @param which a vector identifying which tables to return from within the document (when such was specified). Either a numeric vector for the tables' rank or a character vector specifiying an XPath for the tables.
#' @return A list of table nodes.


rm_nuisance <- function(table.Node, rm_superscript = T, rm_footnotes = T){

  if(isTRUE(rm_superscript)){
    invisible(XML::removeNodes(XML::getNodeSet(table.Node, "//sup")))
  }

  if(isTRUE(rm_footnotes)){
    invisible(XML::removeNodes(XML::getNodeSet(table.Node, "//tfoot")))
  }

  return(table.Node)
}
