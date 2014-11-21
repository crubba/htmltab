#' Extract body cell values
#'
#' @param body a cha
#' @param Node the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process
#' @return logical
#' @examples
#' add(1, 1)
#' add(10, 1)
get_cells <- function(table.Node, body) {

  cell.xpath <- get_cell_xpath(table.Node = table.Node, body = body)

  cells <- unlist(lapply(1:length(cell.xpath), function(xpath) {
    XML::xpathSApply(table.Node, cell.xpath[[xpath]])
  }))

  return(cells)
}



#' Construct an XPath expression for the body cells
#'
#' @param Node the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process
#' @param body
#' @return a character vector wth XPath statement
get_cell_xpath <- function(table.Node, body){

  if(is.character(body)){
    cell.xpath <- body
  }

  tbody <- has_tag(table.Node, "tbody") #does table node has tbody?, list
  td <- has_tag(table.Node, "td") #does table node has <td> tags?, list

  if (is.numeric(body) && tbody){ #check to have header checked (cell.xpath <- sprintf("tr[position() > %s]", max(header)))
    cell.xpath <- lapply(body, function(x) sprintf("tbody/tr[position() = %s]", x))
    cell.xpath <- paste(cell.xpath, collapse= " | ")
  }
  if (is.numeric(body) && td){
    cell.xpath <- lapply(body, function(x) sprintf("tr[position() = %s]", x)) #used to be "tr[td][position() = %s]"
    cell.xpath <- paste(cell.xpath, collapse= " | ") # does it work for 1:4, -1?
  }
  if (is.null(body) && tbody && td) {
    cell.xpath <- "tbody/tr"
  }
  if (is.null(body) && td) {
    cell.xpath <- "tr[td]"
  }

  return(cell.xpath)
}



#' Assert a specific tag in an XML node
#'
#' @param Node the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process
#' @param tag a character string for the tag name to be matched
#' @return logical
#' @examples
#' add(1, 1)
#' add(10, 1)
has_tag <- function(table.Node, tag) {
  x <- unlist(XML::xpathSApply(table.Node, "//*", XML::xmlName)) #probably wrong
  any(x == tag) #tag %in% x
}

#' Fill cell with top-side neighbor value
#'
#' @param Node the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process
#' @param tag a character string for the tag name to be matched
#' @return logical
#' @examples
#' add(1, 1)
#' add(10, 1)
top_value <- function(table.Node){}


#' Fill cell with left-side neighbor value
#'
#' @param Node the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process
#' @param tag a character string for the tag name to be matched
#' @return logical
#' @examples
#' add(1, 1)
#' add(10, 1)
left_value <- function(table.Node){}
