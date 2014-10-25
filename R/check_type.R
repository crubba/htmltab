#Check type of doc and num
check_type <- function(doc, num){

  #URL input
  if(is.character(doc)) {
#    if(!grepl("^www|^http", doc)) stop("Invalid 'doc' input: String is no URL") #doesnt work for files saved on hard drive
    Node <- XML::htmlParse(doc)
    } else if(class(doc) == "HTMLInternalDocument") {
      Node <- doc }

  #Check if table xpath specified
  if(is.character(num)) {
    Node <- XML::getNodeSet(Node, path = num)
  } else {
    if(is.numeric(num)){
      Node <- XML::getNodeSet(Node, path = sprintf("//table[%i]", num))
    } else {
      stop("Invalid 'doc' input: Unkown type")
    }
  }

  return(Node)
}
