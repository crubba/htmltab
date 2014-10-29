#Retrieve Head
get_head <- function(table.Node, header) {

  #Produce XPATH
  header.xpath <- get_xpath_header(table.Node, header = header) #return: vector (char, 1-)

  #Retrieve header elements, account for weird header structures
  head <- lapply(1:length(header.xpath), function(xp) {
    xpath.return <- XML::xpathSApply(table.Node, header.xpath[xp]) #list
    return(xpath.return)
  }
  ) %>% unlist

  return(head)
}


#Assess which XPath to use
get_xpath_header <- function(table.Node, header){

  if(is.character(header)){
    header.xpath = header
  }

  if(is.numeric(header)) {
    header.xpath <- sapply(1:length(header), function(pos) sprintf("position() = %s", header[pos])) %>% paste(., collapse = " or ")
    header.xpath <- sprintf("tr[%s]", header.xpath)
  }

  thead <- has_tag(table.Node, "thead") #check if has thead, list
  th <- has_tag(table.Node, "th") #check if has th, list

  if(thead && is.null(header)) { #check sequence of statements
    header.xpath <- "thead/tr"}

  if (!thead && is.null(header) && th){
    header.xpath <- "tr[th]"} #doesnt work with NZ toplevel, needs */tr[th]


  # if(exists(header.xpath)) {
  # warnings("No usuable header information. Skipping header generation")
  # header.name.table <- vector()
  # return(header.name.table)
  # }

  return(header.xpath)
}

