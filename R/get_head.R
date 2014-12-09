#' Assemble Check input values
#'
#' @param table.Node the table node
#' @param header an information on the header rows
#' @return A list of header information

get_head <- function(table.Node, header) {

  #Produce XPATH
  header.xpath <- get_header_xpath(table.Node, header = header) #return: vector (char, 1-)

  #Retrieve header elements, account for weird header structures
  head <- unlist(lapply(1:length(header.xpath), function(xp) {
    xpath.return <- XML::xpathSApply(table.Node, header.xpath[xp]) #list
    return(xpath.return)
  }
  ))

  if(is.empty(head)){
    warning("No header generated. Try passing information to the header argument")
  }

  return(head)
}


#' Assemble an XPath expression for the header
#'
#' @param table.Node the table node
#' @param header an information on the header rows
#' @return a character vector for the XPath statement
get_header_xpath <- function(table.Node, header){

  if(is.character(header)){
    header.xpath = header
    return(header.xpath)
  }

  if(is.numeric(header)) {
    header.xpath <- sapply(1:length(header), function(pos) sprintf("position() = %s", header[pos]))
    header.xpath <- paste(header.xpath, collapse = " or ")
    header.xpath <- sprintf("tr[%s]", header.xpath)
    return(header.xpath)
  }

  thead <- has_tag(table.Node, "thead") #check if has thead, list
  th <- has_tag(table.Node, "th") #check if has th, list

  if(thead && is.null(header)) { #check sequence of statements
    header.xpath <- "thead/tr"
  }

  if (!thead && is.null(header) && th){
    header.xpath <- "tr[th and not(./td)]" #*/tr[th]
  }

  return(header.xpath)
}
