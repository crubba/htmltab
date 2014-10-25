#Retrieve Head
get_head <- function(Node, header) {

  #Produce XPATH
  header.xpath <- get_xpath_header(Node, header = header) #return: vector (char, 1-)

  #Retrieve header elements, account for weird header structures
  head <- lapply(1:length(header.xpath), function(xp) {
    xpath.return <- XML::xpathSApply(Node[[1]], header.xpath[xp])
    return(xpath.return)
  }
  ) %>% unlist

  return(head)
}


#Assess which XPath to use
get_xpath_header <- function(Node, header){

  if(is.character(header)){
    header.xpath = header}

  if(is.numeric(header)) {
    header.xpath <- sapply(1:length(header), function(pos) sprintf("position() = %s", header[pos])) %>% paste(., collapse = " or ")
    header.xpath <- sprintf("tr[%s]", header.xpath)}

  thead <- has_tag(Node[[1]], "thead") #check if has thead
  th <- has_tag(Node[[1]], "th") #check if has th

  if(thead && is.null(header)) { #check sequence of statements
    header.xpath <- "thead/tr"}

  if (!thead && is.null(header) && th){
    header.xpath <- "tr[th]"}

  # if(exists(header.xpath)) {
  # warnings("No usuable header information. Skipping header generation")
  # header.name.table <- vector()
  # return(header.name.table)
  # }

  return(header.xpath)
}

