check_type <- function(doc, which, ...){

  #Nodeset
  if(any(class(doc) == "XMLNodeSet")){
    Node <- xmlDoc(doc[[1]])
    return(Node)
  }

  #XML input
  if(any(class(doc) == "HTMLInternalDocument")) {
    Node <- doc
    }

  #URL input
  if(is.character(doc)) {
    Node <- XML::htmlParse(doc, ...)
  }

  if(is.null(which)){
    warning("Argument 'which' left unspecified. Choosing first table.", call. = FALSE)
    Node <- XML::getNodeSet(Node, path = "//table")[[1]]
    Node <- XML::xmlDoc(Node)
    return(Node)
  }

  if (is.numeric(which)) {
    Node <- XML::getNodeSet(Node, path = "//table")[[which]] #needs to work for vector which input
    Node <- XML::xmlDoc(Node)
    return(Node)
  }

  if (is.character(which)) {
    xpath <- paste(which, collapse = " | ")
    Node <- XML::getNodeSet(Node, path = xpath) #needs to work for vector which input
    Node <- XML::xmlDoc(Node[[1]])
    return(Node)
  }

  if(!exists("Node")) stop("doc is of unknown type", call. = FALSE)
}
