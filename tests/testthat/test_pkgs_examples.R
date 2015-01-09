context("Manual examples")

test_that("Bayern Munich", {

  doc = "~/Dropbox/htmltable/misc/pages/bayern.html"
  which = "/html/body/div[3]/div[3]/div[4]/table[10]"
  fcb <- htmltable(doc = doc, which = which, header = 0, encoding = "UTF-8", colNames = c("name", "function"))

  expect_that(fcb[1,1], equals("Pep Guardiola"))
  expect_that(fcb[1,2], equals("Head coach"))

  expect_that(colnames(fcb)[1], equals("name"))
  expect_that(colnames(fcb)[2], equals("function"))
})

test_that("Browser", {

  url = "~/Dropbox/htmltable/misc/pages/usage.html"
  doc = XML::htmlParse(url)
  which = "//table[5]"
  #header = 1:2
  bodyFun <- function(node) XML::xmlValue(node) %>% stringr::str_replace(., '%$', '')
  browser <- htmltable(doc = doc, which = which, header = 1:2, encoding = "UTF-8")

  expect_that(browser[1,1], equals("July 2013"))
  expect_that(browser[1,2], equals("20.27%"))

  expect_that(colnames(browser)[1], equals("Date"))
  expect_that(colnames(browser)[2], equals("Internet Explorer"))
})
