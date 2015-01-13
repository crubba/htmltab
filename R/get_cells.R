#' Extract body cell values
#'
#' @param table.Node the table node
#' @param body body identifying information
#' @return list of body information
#' @family get_head
get_cells <- function(table.Node, body) {

  cells <- XML::xpathSApply(table.Node, path = body)

  if(is_empty(cells)){
    stop(sprintf("No body generated. Body is empty. Try passing information to the body argument. Body XPath was '%s'.", body), call. = FALSE)
  }

  return(cells)
}




#' Assert a specific tag in an XML node
#'
#' @param table.Node the table node
#' @param tag a character string for the tag name to be matched
#' @return logical value showing whether there is such a tag

has_tag <- function(table.Node, tag) {
  x <- XML::xpathSApply(table.Node, tag)
  if(length(x) > 0){TRUE} else{FALSE}
}
