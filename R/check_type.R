#' Produce the table node
#'
#' @param doc the HTML document which can be a file name or a URL or an already parsed document
#'   (by XML's parsing functions)
#' @param which a vector of length one for identification of the table in the document. Either
#'    a numeric vector for the tables' rank or a character vector that describes an XPath for the table
#' @param ... additional arguments passed to htmlParse
#' @return a table node
check_type <- function(doc, which, ...){

  #Nodeset
  if(any(class(doc) == "XMLNodeSet")){
    Node1 <- eval.parent(substitute(XML::xmlParse(XML::saveXML(doc[[1]]), list(...))))
    return(Node1)
  }

  #parsed HTML
  if(any(class(doc) == "HTMLInternalDocument")) {
    Node <- doc
    }

  #URL input
  if(is.character(doc)) {
    Node <- eval.parent(substitute(XML::htmlParse(doc, list(...))))
  }

  if(!exists("Node")) stop("doc is of unknown type", call. = FALSE)

  if(is.null(which)){
    warning("Argument 'which' left unspecified. Choosing first table.", call. = FALSE)
    Node1 <- XML::getNodeSet(Node, path = "//table")
    if(length(Node1) == 0){
      stop("Couldn't find a table.", call. = FALSE)
    }

    Node1 <- XML::xmlParse(XML::saveXML(Node1[[1]]))
    return(Node1)
  }

  if (is.numeric(which)) {
    Node1 <- XML::getNodeSet(Node, path = "//table")
    if(length(Node1) <= which){
      stop("Couldn't identify table. Try passing (a different) information to the which argument.", call. = FALSE)
    }

    Node1 <- XML::xmlParse(XML::saveXML(Node1[[which]]))
    return(Node1)
  }

  if (is.character(which)) {
    xpath <- paste(which, collapse = " | ")
    Node1 <- XML::getNodeSet(Node, path = xpath)
    if(is.null(Node1[[1]])){
      stop("Couldn't identify table. Try passing (a different) information to the which argument.", call. = FALSE)
    }

    Node1 <- XML::xmlParse(XML::saveXML(Node1[[1]]))
    return(Node1)
  }

}
