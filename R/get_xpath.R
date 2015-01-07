
#' Assemble XPath expressions for header and body
#'
#' @param table.Node the table node
#' @param header an information for the header rows
#' @param body an information for the body rows
#' @return a character vector of XPath statements
get_xpath <- function(table.Node, header, body){

  if(is.character(header) && is.character(body)){
    xp = c(header.xpath = header, body.xpath = body)
    return(xp)
  }

  thead <- has_tag(table.Node, "thead")
  thead.th <- has_tag(table.Node, "thead/tr[th]")
  thead.td <- has_tag(table.Node, "thead/tr[td]")

  tbody <- has_tag(table.Node, "tbody")
  tbody.th <- has_tag(table.Node, "tbody/tr[th]")
  tbody.td <- has_tag(table.Node, "tbody/tr[td]")

  tr <- has_tag(table.Node, "tr")
  th <- has_tag(table.Node, "tr[th]")
  td <- has_tag(table.Node, "tr[td]")



  #HEADER
  if(is.character(header)){
    header.xpath = header
  } else{

    if(is.numeric(header)) {
      if(thead | tbody){
        header.count <- header - 1
        header.xpath <- sapply(1:length(header.count), function(pos) {
          sprintf("count(preceding::tr) = %s", header.count[pos])
          }) #preceding-sibling?
        header.xpath <- paste(header.xpath, collapse = " or ")
        header.xpath <- c(sprintf("//tr[%s]", header.xpath)) #//, */. tr
      } else{
        header.count <- header - 1
        header.xpath <- sapply(1:length(header.count), function(pos) sprintf("count(preceding-sibling::tr) = %s", header.count[pos]))
        header.xpath <- paste(header.xpath, collapse = " or ")
        header.xpath <- c(sprintf("tr[%s]", header.xpath)) #, sprintf("and not(%s)", header.xpath))
      }
    } else {

      if(!is.null(header)) {warning("You can only pass numeric and character vectors to header.")}

      if(thead) { #If a thead exists, take these rows (independent if they are td or th)
        header.xpath <- c('thead/tr')  #, 'and not(ancestor::thead)')
      }

      if (!thead && th){
        header.xpath <- "tr[th and not(./td)]"
      }

      if (!thead && !th){
        header.xpath <- c("tr[1]") #, "and not(1)")
        warning("Neither <thead> nor <th> information found. Taking first table row. If incorrect, specifiy header argument", call. = FALSE)
      }

    }
  }


  #BODY

  if(is.character(body)){
    body.xpath = body
    xp = c(header.xpath, body.xpath)
    return(xp)
  } else{

    if(is.numeric(body)){

      if(tbody){
      body.index <- body - 1
      body.xpath <- sapply(1:length(body.index), function(pos) sprintf("count(preceding::tr) = %s", body.index[pos]))
      body.xpath <- paste(body.xpath, collapse = " or ")
      body.xpath <- sprintf("*/tr[%s] | tr[%s]", body.xpath, body.xpath) #control for different hierarchical structure, should include check for tbody
    } else{
      body.index <- body - 1
      body.xpath <- sapply(1:length(body.index), function(pos) sprintf("count(preceding-sibling::tr) = %s", body.index[pos]))
      body.xpath <- paste(body.xpath, collapse = " or ")
      body.xpath <- sprintf("*/tr[%s] | tr[%s]", body.xpath, body.xpath)
    }
    } else {

    if(tbody){
      body.xpath <- "tbody/tr" #[ancestor::tbody]
    } else {
      body.xpath <- "tr[./td]"
    }

    }
  }

  xp <- c(header.xpath, body.xpath)
  return(xp)}
