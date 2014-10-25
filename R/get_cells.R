#Assert if table has <tag>
has_tag <- function(Node, tag) {
  x <- unlist(XML::xpathSApply(Node, "//*", XML::xmlName)) #probably wrong
  any(x == tag) #tag %in% x
}


#get cell xpath
get_cell_xpath <- function(body, Node){

  if(is.character(body)){
    cell.xpath <- body}

  tbody <- has_tag(Node[[1]], "tbody") #does table node has tbody?
  td <- has_tag(Node[[1]], "td") #does table node has <td> tags?

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
    XML::xpathSApply(Node[[1]], cell.xpath[[xpath]])
  })  %>% unlist

  return(cells)
}

