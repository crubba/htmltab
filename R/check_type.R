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

    if(is.null(which)){
      warning("Argument 'which' left unspecified. Choosing first table.")
      which <- 1
    }

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
