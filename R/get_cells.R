
#' Assert a specific tag in an XML node
#'
#' @param Node the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process
#' @param tag a character string for the tag name to be matched
#' @return logical
#' @examples
#' add(1, 1)
#' add(10, 1)
has_tag <- function(Node, tag) {
  x <- unlist(XML::xpathSApply(Node, "//*", XML::xmlName)) #probably wrong
  any(x == tag) #tag %in% x
}


#' Construct an XPath expression for the body cells
#'
#' @param body a cha
#' @param Node the HTML document which can be a file name or a URL or an already parsed HTMLInternalDocument, or an HTML node of class XMLInternalElementNode, or a character vector containing the HTML content to parse and process
#' @return logical
#' @examples
#' add(1, 1)
#' add(10, 1)
get_cell_xpath <- function(body, Node){

  if(is.character(body)){
    cell.xpath <- body}

  tbody <- has_tag(Node, "tbody") #does table node has tbody?, list
  td <- has_tag(Node, "td") #does table node has <td> tags?, list

  if (is.numeric(body) && tbody){ #check to have header checked (cell.xpath <- sprintf("tr[position() > %s]", max(header)))
    cell.xpath <- lapply(body, function(x) sprintf("tbody/tr[position() = %s]", x)) %>% paste(., collapse= " | ")
  }
  if (is.numeric(body) && td){
    cell.xpath <- lapply(body, function(x) sprintf("tr[td][position() = %s]", x)) %>% paste(., collapse= " | ") # does it work for 1:4, -1?
  }
  if (is.null(body) && tbody) {
    cell.xpath <- "tbody/tr"
  }
  if (is.null(body)) {
    cell.xpath <- "tr[td]"}

  return(cell.xpath)
}

#Extracts cell values
get_cells <- function(Node, body) {

  cell.xpath <- get_cell_xpath(body = body, Node = Node)

  cells <- lapply(1:length(cell.xpath), function(xpath) {
    XML::xpathSApply(Node, cell.xpath[[xpath]])
  })  %>% unlist

  return(cells)
}

