#Extracts the cell XML value (default)
get_cell_element <- function(cells, tag = "td | th", elFun = elFun) { #"td for cell values, th for header values

  cell.element <- lapply(cells, function(tr) {
    XML::xpathSApply(tr, tag, elFun)
  }
  )

  return(cell.element)
}


#Extracts rowspan information
get_rowspans <- function(cells, tag = "td | th"){

  rowspans <- lapply(cells, function(tr) {
    XML::xpathSApply(tr, tag, function(node) {
      rs <- XML::xmlGetAttr(node, "rowspan")
      value <- as.numeric(ifelse(is.null(rs), 1, rs))
      return(value)
    }
    )
  }
  )
  return(rowspans)
}


#Extracts colspan information
get_colspans <- function(cells, tag = "td | th"){

  colspans <- lapply(cells, function(tr) {
    XML::xpathSApply(tr, tag, function(node) {
      cs <- XML::xmlGetAttr(node, "colspan")
      value <- as.numeric(ifelse(is.null(cs), 1, rs))
      return(value)
    }
    )
  }
  )
  return(colspans)
}
