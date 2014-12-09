#' Extract body cell values
#'
#' @param table.Node the table node
#' @param body body identifying information
#' @return list of body information
#' @family get_head
get_cells <- function(table.Node, body) {

  cell.xpath <- get_cell_xpath(table.Node = table.Node, body = body)

  cells <- unlist(lapply(1:length(cell.xpath), function(xpath) {
    XML::xpathSApply(table.Node, cell.xpath[[xpath]])
  }))

  if(is.empty(cells)){
    stop("No body generated. Try passing information to the body argument")
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

  tbody <- has_tag(table.Node, "tbody") #does table node has tbody?, list
  td <- has_tag(table.Node, "td") #does table node has <td> tags?, list

  if (is.numeric(body) && tbody){ #check to have header checked (cell.xpath <- sprintf("tr[position() > %s]", max(header)))
    cell.xpath <- lapply(body, function(x) sprintf("tbody/tr[position() = %s]", x))
    cell.xpath <- paste(cell.xpath, collapse= " | ")
    return(cell.xpath)
  }
  if (is.numeric(body) && td){
    cell.xpath <- lapply(body, function(x) sprintf("tr[position() = %s]", x)) #used to be "tr[td][position() = %s]"
    cell.xpath <- paste(cell.xpath, collapse= " | ") # does it work for 1:4, -1?
    return(cell.xpath)
  }
  if (is.null(body) && tbody && td) {
    cell.xpath <- "tbody/tr"
  }
  if (is.null(body) && td && !(tbody)) { # && is.numeric(header)
    cell.xpath <- "tr[td]" #CORRECT?
  }
  if(!exists("cell.xpath")){
    cell.xpath <- "tr[td]"
  }

#  if(is.null(body) && td && !(tbody) && is.numeric(header)){
#    header.xpath <- sapply(1:length(header), function(pos) sprintf("not(position() = %s)", header[pos]))
#    header.xpath <- paste(header.xpath, collapse = " or ")
#    cell.xpath <- sprintf("tr[td and %s]", header.xpath)
#  } else {
#    cell.xpath <- "tr[td]"
#  }

  return(cell.xpath)
}



#' Assert a specific tag in an XML node
#'
#' @param table.Node the table node
#' @param tag a character string for the tag name to be matched
#' @return logical value showing whether there is such a tag

has_tag <- function(table.Node, tag) {
  x <- unlist(XML::xpathSApply(table.Node, "//*", XML::xmlName)) #probably wrong
  any(x == tag) #tag %in% x
}
