#' Assemble Check input values
#'
#' @param table.Node the table node
#' @param header an information on the header rows
#' @return A list of header information

get_head <- function(table.Node, header) {

  #header.xpath <- get_header_xpath(table.Node, header = header)

  #Retrieve header elements, account for weird header structures
  head <- XML::xpathSApply(table.Node, path = header)
#  head <- unlist(lapply(1:length(header.xpath), function(xp) {
#    xpath.return <- XML::xpathSApply(table.Node, header.xpath[xp]) #list
#    return(xpath.return)
#  }
#  ))

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

  thead <- has_tag(table.Node, "//thead")
  thead.th <- has_tag(table.Node, "//thead[th]")
  thead.td <- has_tag(table.Node, "//thead[td]")
  th <- has_tag(table.Node, "//th")

  if(is.numeric(header)) {
    if(thead){
      header.count <- header - 1
      header.xpath <- sapply(1:length(header.count), function(pos) sprintf("count(preceding::tr) = %s", header.count[pos]))
      header.xpath <- paste(header.xpath, collapse = " or ")
      header.xpath <- c(sprintf("*/tr[%s]", header.xpath), sprintf("and not(%s)", header.xpath))
    } else{
      header.count <- header - 1
      header.xpath <- sapply(1:length(header.count), function(pos) sprintf("count(preceding-sibling::tr) = %s", header.count[pos]))
      header.xpath <- paste(header.xpath, collapse = " or ")
      header.xpath <- c(sprintf("tr[%s]", header.xpath), sprintf("and not(%s)", header.xpath))
    }

    return(header.xpath)
  }

  if(is.null(header)){

    if(thead) { #If a thead exists, take these rows (independent if they are td or th)
      header.xpath <- c('thead/tr', 'and not(ancestor::thead)')
      return(header.xpath)
    }

    if (!thead && th){
    header.xpath <- "tr[th and not(./td)]"
    return(header.xpath)
    }

    if (!thead && !th){
      header.xpath <- c("tr[1]", "and not(1)")
      warning("Neither <thead> nor <th> information found. Taking first table row. If incorrect, specifiy header argument", call. = FALSE)
      return(header.xpath)
    }
  }

}



# if(is.numeric(header) && !thead) {
#   header.xpath <- sapply(1:length(header), function(pos) sprintf("position() = %s", header[pos]))
#   header.xpath <- paste(header.xpath, collapse = " or ")
#   header.xpath <- sprintf("tr[%s]", header.xpath)
#   return(header.xpath)
# }
