read_charset <- function(doc){
  docMeta <- XML::htmlParse(doc)
  charset <- XML::xpathSApply(docMeta, "//meta[@charset]", XML::xmlValue)
  charset
}
