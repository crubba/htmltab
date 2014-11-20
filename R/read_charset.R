read_charset <- function(Node){
  charset <- xpathSApply(Node, "//meta[@charset]", xmlValue)
  charset
}
