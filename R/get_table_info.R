#' Extracts cells elements
#'
#' @param cells a list of cell nodes
#' @param tag a character vector that provides information used in the XPath expression to extract the correct elements
#' @param elFun a function that is executed over the body cell nodes
#' @param rm_escape a character vector that, if specified, is used to replace escape sequences in header and body cells (default value ' ')
#' @return the body element

get_cell_element <- function(cells, tag = "td | th", elFun, rm_escape = NULL) { #"td for cell values, th for header values

  cell.element <- lapply(cells, function(tr) {
    XML::xpathSApply(tr, tag, elFun)
  })

  if(!is.null(rm_escape)) {
    cell.element <- lapply(cell.element, function(el) gsub("([[:alpha:]])-[\b\n\t\r]([[:alpha:]])", "\\1\\2", el))
    cell.element <- lapply(cell.element, function(el) gsub("[\b \n \t \r]", rm_escape, el))
  }
  return(cell.element)
}


#' Extracts rowspan information
#'
#' @param cells a list of cell nodes
#' @param tag a character vector that provides information used in the XPath expression to extract the correct elements
#' @return A list of row information from the cells
get_rowspans <- function(cells, tag = "td | th"){

  rowspans <- lapply(cells, function(tr) {
    XML::xpathSApply(tr, tag, function(node) {
      rs <- XML::xmlGetAttr(node, "rowspan")
      value <- as.numeric(ifelse(is.null(rs), 1, rs))
      return(value)
    })
  })
  return(rowspans)
}


#' Extracts colspan information
#'
#' @param cells a list of cell nodes
#' @param tag a character vector that provides information used in the XPath expression to extract the correct elements
#' @return A list of column information from the cells
get_colspans <- function(cells, tag = "td | th"){

  colspans <- lapply(cells, function(tr) {
    XML::xpathSApply(tr, tag, function(node) {
      cs <- XML::xmlGetAttr(node, "colspan")
      value <- as.numeric(ifelse(is.null(cs), 1, cs))
      return(value)
    })
  })
  return(colspans)
}

#' Extracts header elements
#'
#' @param cells a list of cell nodes
#' @param tag a character vector that provides information used in the XPath expression to extract the correct elements
#' @return A list of header information from the cells
get_header_elements <- function(cells, tag = "td | th"){

  header_elements <- lapply(cells, function(tr) {
    XML::xpathSApply(tr, tag, function(node) {
      if(XML::xmlName(node) != "sup") {
      value <- XML::xmlValue(node)
      }
      return(value)
    })
  })
  return(header_elements)
}
