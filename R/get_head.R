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

  thead <- has_tag(table.Node, "thead") #check if has thead, list
  th <- has_tag(table.Node, "th") #check if has th, list

  if(is.numeric(header)) {
    header.count <- header - 1
    header.xpath <- sapply(1:length(header.count), function(pos) sprintf("count(preceding-sibling::tr) = %s", header.count[pos]))
    header.xpath <- paste(header.xpath, collapse = " or ")
    header.xpath <- sprintf("tr[%s]", header.xpath)
    return(header.xpath)
  }
  if(thead && is.null(header)) { #If a thead exists, take these rows (independent if they are td or th)
    header.xpath <- "thead/tr" #[th and not(./td)]
    return(header.xpath)
  }

  if (th && !thead && is.null(header)){
    header.xpath <- "tr[th and not(./td)]" #*/tr[th]
    return(header.xpath)
  }

  if (!th && !thead && is.null(header)){
    header.xpath <- "tr[1]"
    warning("Neither <thead> nor <th> information found. Taking first table row. If incorrect, specifiy header argument")
    return(header.xpath)
  }

  return(header.xpath)
}



# if(is.numeric(header) && !thead) {
#   header.xpath <- sapply(1:length(header), function(pos) sprintf("position() = %s", header[pos]))
#   header.xpath <- paste(header.xpath, collapse = " or ")
#   header.xpath <- sprintf("tr[%s]", header.xpath)
#   return(header.xpath)
# }
