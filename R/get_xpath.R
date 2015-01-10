#' Produce XPath for numeric index
#'
#' @param index numeric vector
#' @return a character vector of XPath statements
num_xpath <- function(index){
  index.count <- index - 1
  index.xpath <- sapply(1:length(index.count), function(pos) {
    sprintf("count(preceding::tr) = %s", index.count[pos])
  })
  index.xpath <- paste(index.xpath, collapse = " or ")
  index.xpath <- c(sprintf("//tr[%s]", index.xpath), sprintf("not(%s)", index.xpath))
  return(index.xpath)
}

#' Return header xpath
#'
#' @param table.Node the table node
#' @param header an information for the header rows
#' @return a character vector of XPath statements
get_head_xpath <- function(table.Node, header){

  thead <- has_tag(table.Node, "//thead")
  thead.th <- has_tag(table.Node, "//thead/tr[th]")
  thead.td <- has_tag(table.Node, "//thead/tr[td]")

  tr <- has_tag(table.Node, "//tr")
  th <- has_tag(table.Node, "//tr[th and not(./td)]")
  td <- has_tag(table.Node, "//tr[td and not(./th)]")

  if(is.character(header)){
    header.xpath <- c(header, "ancestor::tr")
    return(header.xpath)
  }

  if(is.integer(header)) {
    header.xpath <- num_xpath(index = header)
    return(header.xpath)
  }

  if(thead) {
    header.xpath <- c('//thead/tr[%s]', 'not(ancestor::thead)')
    return(header.xpath)
  }

  if (!thead && th){
    header.xpath <- c("//tr[th and not(./td) and %s]", "./td")
    return(header.xpath)
  }

  if (!thead && !th){
    header.xpath <- c("//tr[position() = 1 and %s]", "not(position() = 1)")
    warning("Neither <thead> nor <th> information found. Taking first table row. If incorrect, specifiy header argument.", call. = FALSE)
    return(header.xpath)
  }
}


#' Return header xpath
#'
#' @param table.Node the table node
#' @param body an information for the body rows
#' @return a character vector of XPath statements
get_body_xpath <- function(table.Node, body){

  tbody <- has_tag(table.Node, "//tbody")
  tbody.th <- has_tag(table.Node, "//tbody/tr[th]")
  tbody.td <- has_tag(table.Node, "//tbody/tr[td]")

  tr <- has_tag(table.Node, "//tr")
  th <- has_tag(table.Node, "//tr[th and not(./td)]")
  td <- has_tag(table.Node, "//tr[td and not(./th)]")

  if(is.character(body)){
    body.xpath <- c(body, "acenstor::tr")
    return(body.xpath)
  }

  if(is.integer(body)){
    body.xpath <- num_xpath(index = body)
    return(body.xpath)
  }

  if(tbody){
    body.xpath <- c("//tbody/tr[%s]", "not(ancestor::tbody)")
    return(body.xpath)
  } else {
    body.xpath <- c("//tr[./td and %s]", "*")
    return(body.xpath)
  }

}

#' Assemble XPath expressions for header and body
#'
#' @param table.Node the table node
#' @param header an information for the header rows
#' @param body an information for the body rows
#' @return a character vector of XPath statements
get_xpath <- function(table.Node, header, body){

  #Coerce body and header into integer
  if(is.numeric(header)){
    header <- as.integer(header)
  }

  if(is.numeric(body)){
    body <- as.integer(body)
  }

  classes <- sapply(list(header, body), class)

  if(!all(classes %in% c("integer", "character", "NULL"))){
    stop("body and/or header argument is of unknown class.", call. = FALSE)
  }

  if(all(classes %in% c("integer", "character"))){

    if(is.integer(header)){
      header.xpath <- num_xpath(index = header)[1]
    } else {
      header.xpath <- header
    }

    if(is.integer(body)){
      body.xpath <- num_xpath(index = body)[1]
    } else {
      body.xpath <- body
    }

    xp = c(header.xpath = header.xpath, body.xpath = body.xpath)
    return(xp)
  }

  #produce xpaths
  header.xpath <- get_head_xpath(table.Node = table.Node, header = header)
  body.xpath <- get_body_xpath(table.Node = table.Node, body = body)

  #Complementary header and body
  header.xpath[1] <- sprintf(header.xpath[1], body.xpath[2])
  body.xpath[1] <- sprintf(body.xpath[1], header.xpath[2])

  xp <- c(header.xpath[1], body.xpath[1])
  return(xp)
}
