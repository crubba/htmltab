context("Header dimension checks")

doc <- "http://en.wikipedia.org/wiki/Demography_of_the_United_Kingdom"
header = NULL;

Node <- check_type(doc, which)
table.Node <- Node[[1]]
head <- get_head(table.Node = table.Node, header = header)
header.colspans <- get_colspans(head, tag = "td | th") #colspans for relevant row values
header.rowspans <- get_rowspans(head, tag = "td | th") #rowspans so far ignored
header.names <- get_cell_element(head, tag = "td | th", elFun = function(node)XML::xmlValue(node))

test_that("row and column span lists have same dimensions", {

  expect_true(
    all(c(length(header.rowspans), length(header.colspans), length(header.names)) == 3)
    )

})
