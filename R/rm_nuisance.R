#' Remove nuisance elements
#'
#' @param table.Node the table node
#' @param rm_superscript logical, denotes whether superscript information should be removed from header and body cells (default value TRUE)
#' @param rm_footnotes logical, denotes whether semantic footer information should be removed (default value TRUE)
#' @param rm_invisible logical, should nodes that are not visible (display:none attribute) be removed?
#' @return A list of table nodes.
rm_nuisance <- function(table.Node, rm_superscript, rm_footnotes, rm_invisible){

  if(isTRUE(rm_superscript)){
    invisible(XML::removeNodes(XML::getNodeSet(table.Node, "//sup")))
  }

  if(isTRUE(rm_footnotes)){
    invisible(XML::removeNodes(XML::getNodeSet(table.Node, "//tfoot")))
  }

  if(isTRUE(rm_invisible)){
    invisible(XML::removeNodes(XML::getNodeSet(table.Node, "//*[@style = 'display:none']")))
  }

  return(table.Node)
}


#' Remove no data columns
#'
#' @param df a data frame
#' @return a data frame
rm_empty_cols <- function(df){

  empty.cols <- sapply(df, function(col){
    all(grepl("[[:alnum:]]", col) == FALSE)
  })

  rm.these <- which(empty.cols == TRUE)
  no.col.name <- grep('^V[[:digit:]]', colnames(df))
  rm.these <- intersect(rm.these, no.col.name)

  if(length(rm.these) > 0) {
    df <- df[, -rm.these]
  }

  return(df)
}
