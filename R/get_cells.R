#' Extract body cell values
#'
#' @param table.Node the table node
#' @param body body identifying information
#' @return list of body information
#' @family get_head
get_cells <- function(table.Node, body) {

  #cell.xpath <- get_cell_xpath(table.Node = table.Node, body = body)

  cells <- XML::xpathSApply(table.Node, path = body)

  if(is.empty(cells)){
    stop("No body generated. Try passing information to the body argument", call. = FALSE)
  }

  return(cells)
}


#' Construct an XPath expression for the body cells
#'
#' @param table.Node the table node
#' @param body body identifying information
#' @return a character vector wth XPath statement
get_cell_xpath <- function(table.Node, body){

  if(is.character(body)){
    cell.xpath <- body
    return(cell.xpath)
  }

# if(length(head) == 2){
#    header.xpath <- head[2]
#  }

  if(is.numeric(body)) {
    body.index <- body - 1
    body.xpath <- sapply(1:length(body.index), function(pos) sprintf("count(preceding::tr) = %s", body.index[pos]))
    body.xpath <- paste(body.xpath, collapse = " or ")
    cell.xpath <- sprintf("*/tr[%s] | tr[%s]", body.xpath, body.xpath) #control for different hierarchical structure, should include check for tbody
    return(cell.xpath)
  }

  tbody <- has_tag(table.Node, "tbody")

  if(tbody){
    cell.xpath <- "tbody/tr[ancestor::tbody]"
  } else {
    cell.xpath <- "tr[./td]"
  }

  return(cell.xpath)
}



#' Assert a specific tag in an XML node
#'
#' @param table.Node the table node
#' @param tag a character string for the tag name to be matched
#' @return logical value showing whether there is such a tag

has_tag <- function(table.Node, tag) {
  x <- XML::xpathSApply(table.Node, tag)
  if(length(x) > 0){TRUE} else{FALSE}
  #x <- unlist(XML::xpathSApply(table.Node, "//*", XML::xmlName)) #probably wrong
  #any(x == tag) #tag %in% x
}
