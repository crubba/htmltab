#' Robust methods for extracting structured information out of HTML tables
#'
#' @export
#' @param doc the HTML document which can be a file name or a URL or an already parsed document (by XML's parsing functions)
#' @param which a vector of length one for identification of the table in the document. Either a numeric vector for the tables' rank or a character vector that describes an XPath for the table
#' @param header a vector that contains information for the identification of the header row(s). A numeric vector can be specified where each element corresponds to the table rows. A character vector may be specified that describes an XPath for the header rows. If left unspecified, htmltable tries to use semantic information from the HTML code
#' @param headerSep a character vector that is used as a seperator in the construction of the table's variable names (default value ' >> ')
#' @param fillNA logical, if TRUE empty cells are filled with NA (default TRUE)
#' @param body a vector that specifies which table rows should be used as body information. A numeric vector can be specified where each element corresponds to a table row. A character vector may be specified that describes an XPath for the body rows. If left unspecified, htmltable tries to use semantic information from the HTML code
#' @param bodyFun a function that is executed over the body cell nodes
#' @param headerFun a function that is executed over the header cell nodes
#' @param rm_superscript logical, denotes whether superscript information should be removed from header and body cells (default value TRUE)
#' @param rm_footnotes logical, denotes whether semantic footer information should be removed (default value TRUE)
#' @param rm_nodata_cols logical, denotes whether columns that have no alphanumeric data should be removed
#' @param rm_escape a character vector that, if specified, is used to replace escape sequences in header and body cells (default value ' ')
#' @param colNames a character vector of column names, or a function that can be used to replace specific column names (default value NULL)
#' @param ... additional arguments
#' @return An R data frame
#' @examples
#'
#'# When no spans are present, htmltable produces output identical to XML's readHTMLTable()
#'
#'  url <- "http://en.wikipedia.org/wiki/World_population"
#'  xp <- "//caption[starts-with(text(),'World historical')]/ancestor::table"
#'  htmltable(doc = url, which = xp)
#'
#'  popFun <- function(node) {
#'    x <- XML::xmlValue(node)
#'    gsub(',', '', x)
#'  }
#'
#'  htmltable(doc = url, which = xp, bodyFun = popFun)
#'
#' #This table lacks header information. We provide them through colNames.
#' #We also need to set header = 0 to indicate that no header is present.
#' doc <- "http://en.wikipedia.org/wiki/FC_Bayern_Munich"
#' xp2 <- "//td[text() = 'Head coach']/ancestor::table"
#' htmltable(doc = doc, which = xp2, header = 0, encoding = "UTF-8", colNames = c("name", "role"))
#'
#' #htmltable recognizes column spans and produces a one-dimension vector of variable information,
#' #also removes automatically superscript information since these are usually not of use.
#'
#'  doc <- "http://en.wikipedia.org/wiki/Usage_share_of_web_browsers"
#'  xp3 <-  "//table[6]"
#'  bFun <- function(node) {
#'    x <- XML::xmlValue(node)
#'    gsub('%$', '', x)
#'  }
#'
#'  htmltable(doc = doc, which = xp3, bodyFun = bFun)
#'


htmltable <- function(doc,
                      which = NULL,
                      header = NULL,
                      headerSep = " >> ",
                      fillNA = T,
                      body = NULL,
                      bodyFun = function(node)XML::xmlValue(node),
                      headerFun = function(node)XML::xmlValue(node),
                      rm_superscript = T,
                      rm_escape = " ",
                      rm_footnotes = T,
                      rm_nodata_cols = T,
                      colNames = NULL,
                      ...){

  # Check Inputs & Clean Up & Add tr --------
  table.Node <- check_type(doc, which, ...)
  table.Node <- rm_nuisance(table.Node = table.Node, rm_superscript = rm_superscript, rm_footnotes = rm_footnotes)
  table.Node <- add_tr(table.Node = table.Node, header =  header, body = body)

  #Produce XPath for header and body
  xpath <- get_xpath(table.Node = table.Node, header = header, body = body)


  # Create Header ---------------------------

  #Retrieve Head Elements
  head <- get_head(table.Node = table.Node, header = xpath[1])

  header.colspans <- get_colspans(head, tag = "td | th")
  header.rowspans <- get_rowspans(head, tag = "td | th")
  header.names <- get_cell_element(head, tag = "td | th", rm_escape = rm_escape, elFun = headerFun)

  #Span head
  header.name.table <- span_header(header.names, header.colspans, header.rowspans, headerSep = headerSep)


  # Create Body ---------------------------

  #Get Body Cell Nodes
  cells <- get_cells(table.Node = table.Node, body = xpath[2])

  #Extract and transform body cell elements
  vals <- get_cell_element(cells, elFun = bodyFun, rm_escape = rm_escape)

  #Produce rowspans and colspans lists from body cell
  body.rowspans <- get_rowspans(cells)
  body.colspans <- get_colspans(cells)

  #Produce table body
  tab <- span_body(vals, colspans = body.colspans, rowspans = body.rowspans)


  # Finish ---------------------------

  #Give column Names
  colnames(tab) <- clean_colnames(header.name.table = header.name.table, colNames = colNames)

  #Make df
  tab <- as.data.frame(tab, stringsAsFactors = F)

  #Check if there are no data columns
  if(isTRUE(rm_nodata_cols)){
    tab <- rm_empty_cols(df = tab)
  }

  #Replace empty vals by NA
  if(isTRUE(fillNA)){
    tab[tab == ""] <- NA
  }

  return(tab)
}
