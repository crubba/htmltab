#' Robust methods for extracting structured information out of HTML tables
#'
#' @export
#' @param doc the HTML document which can be a file name or a URL or an already parsed document (by XML's parsing functions
#' @param which a vector identifying which tables to return from the document. Either a numeric vector for the tables' rank (no negative indexes allowed) or a character vector specifiying an XPath for the table
#' @param header a vector specifying header information. A numeric vector can be specified where each element corresponds to the table rows. A character vector may be specified that describes an XPath for the header rows. If left unspecified, htmltable tries to use semantic information from the HTML code
#' @param headerSep a character vector that is used as a seperator in the construction of the table's variable names (default value ' >> ')
#' @param body a vector that specifies which table rows should be used as body information. A numeric vector can be specified where each element corresponds to a table row (no negative indexes allowed). A character vector may be specified that describes an XPath for the body rows. If left unspecified, htmltable tries to use semantic information from the HTML code
#' @param bodyFun a function that is executed over the body cell nodes
#' @param headerFun a function that is executed over the header cell nodes
#' @param rm_superscript logical, denotes whether superscript information should be removed from header and body cells (default value TRUE)
#' @param rm_escape a character vector that, if specified,is used to replace escape sequences in header and body cells (default value ' ')
#' @param rm_footnotes logical, denotes whether semantic footer information should be removed (default value TRUE)
#' @param colNames a character vector of column names, or a function that can be used to replace specific column names (default value NULL)
#' @param ... additional arguments
#' @return An R data frame
#' @examples
#'
#'# When no spans are presented, htmltable produces output identical to XML's readHTMLTable()
#'
#'url <- "http://en.wikipedia.org/wiki/World_population"
#'xp <- "//caption[text() = 'World historical and predicted populations (in millions)']/ancestor::table"
#'htmltable(doc = url, which = xp)
#'
#' library(magrittr)
#' library(stringr)
#' library(XML)
#'popFun <- function(node) xmlValue(node) %>% str_replace(., ',', '')
#'htmltable(doc = url, which = xp, bodyFun = popFun)
#'
#' #This table lacks header information. We provide them through colNames
#' doc <- "http://en.wikipedia.org/wiki/FC_Bayern_Munich"
#' xp2 <- "//td[text() = 'Head coach']/ancestor::table"
#' htmltable(doc = doc, which = xp2, encoding = "UTF-8", colNames = c("name", "role"))
#'
#'
#' #htmltable recognizes column spans and produces a one-dimension vector of variable information,
#' #also removes automatically superscript information since these are usually not wanted.
#'
#' doc <- "http://en.wikipedia.org/wiki/Usage_share_of_web_browsers"
#' xp3 <-  "//table[5]"
#' bFun <- function(node) {xmlValue(node) %>% str_replace(., '%$', '') %>% ifelse(equals(., ''), NA, .)}
#' htmltable(doc = doc, which = xp3, bodyFun = bFun)
#'

htmltable <- function(doc,
                      which = NULL,
                      header = NULL,
                      headerSep = " >> ",
                      body = NULL,
                      bodyFun = function(node)XML::xmlValue(node),
                      headerFun = function(node)XML::xmlValue(node),
                      rm_superscript = T,
                      rm_escape = " ",
                      rm_footnotes = T,
                      colNames = NULL,
                      ...){

  args <- list(...)

  # Check Inputs & Clean Up -----------------
  Node <- check_type(doc, which, ...)

  table.Node <- Node[[1]]

  table.Node <- rm_nuisance(table.Node = table.Node, rm_superscript = rm_superscript, rm_footnotes = rm_footnotes)

  # Create Header ---------------------------

  #Retrieve Head Elements
  head <- get_head(table.Node = table.Node, header = header)

  header.colspans <- get_colspans(head, tag = "td | th")
  header.rowspans <- get_rowspans(head, tag = "td | th")
  header.names <- get_cell_element(head, tag = "td | th", rm_escape = rm_escape, elFun = headerFun)

  #Span head
  header.name.table <- span_header(header.names, header.colspans, header.rowspans, headerSep = headerSep)


  # Create Body ---------------------------

  #Get Body Cell Nodes
  cells <- get_cells(table.Node = table.Node, body = body)

  #Extract and transform body cell elements
  vals <- get_cell_element(cells, rm_escape = rm_escape, elFun = bodyFun)

  #Produce rowspans and colspans lists from body cell
  body.rowspans <- get_rowspans(cells)
  body.colspans <- get_colspans(cells)

  #Produce table body
  tab <- span_body(vals, colspans = body.colspans, rowspans = body.rowspans)


  # Finish ---------------------------

  #Give Column Names
  colnames(tab) <- clean_colnames(header.name.table = header.name.table, colNames = colNames)

  #Make df
  tab <- as.data.frame(tab, stringsAsFactors = F)

  return(tab)
}
