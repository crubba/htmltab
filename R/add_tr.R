#' Assert that all table rows are nested in tr tags
#'
#' @param table.Node the table node
#' @param header information passed from the header argument
#' @param body information passed from the body argument
#' @return the table node

add_tr <- function(table.Node, header, body){

  if(!is.character(header)){

    x <- has_tag(table.Node, "//thead[./th | ./td and not(ancestor::tr)]")

    if(isTRUE(x)){
      old.header <- XML::getNodeSet(table.Node, "//thead/th | //thead/td")
      invisible(new.header <- XML::newXMLNode("thead"))
      invisible(XML::newXMLNode("tr", parent = new.header))
      invisible(XML::addChildren(new.header[["tr"]], old.header))
      invisible(XML::replaceNodes(oldNode = XML::getNodeSet(table.Node, "//thead")[[1]], newNode = new.header))
    }

    x <- has_tag(table.Node, "//tr[./thead]")

    if(isTRUE(x)){
      old.header <- XML::getNodeSet(table.Node, "//tr/thead/th | //tr/thead/td")
      invisible(new.header <- XML::newXMLNode("thead"))
      invisible(XML::newXMLNode("tr", parent = new.header))
      invisible(XML::addChildren(new.header[["tr"]], old.header))
      invisible(XML::replaceNodes(oldNode = XML::getNodeSet(table.Node, "//tr[./thead]")[[1]], newNode = new.header))
    }
  }

  if(!is.character(body)){

    x <- has_tag(table.Node, "//tbody[./th | ./td]")

    if(isTRUE(x)){
      old.body <- XML::getNodeSet(table.Node, "//tbody/th | //tbody/td")
      invisible(new.body <- XML::newXMLNode("tbody"))
      invisible(XML::newXMLNode("tr", parent = new.body))
      invisible(XML::addChildren(new.body[["tr"]], old.body))
      invisible(XML::replaceNodes(oldNode = XML::getNodeSet(table.Node, "tbody")[[1]], newNode = new.body))
    }
  }

  return(table.Node)
}
