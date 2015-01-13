context("htmltab examples work correctly")

test_that("Example 1 works", {

  url <- "http://en.wikipedia.org/wiki/World_population"
  xp <- "//caption[text() = 'World historical and predicted populations (in millions)']/ancestor::table"
  ex1 <- htmltab(doc = url, which = xp)

  expect_that(ex1[1,1], equals("World"))
  expect_that(colnames(ex1)[1], equals("Region"))
})


test_that("Example 2 works", {

  library(XML)

  url <- "http://en.wikipedia.org/wiki/World_population"
  xp <- "//*[text() = 'World historical and predicted populations (in millions)']/ancestor::table"
  popFun <- function(node) {
    x <- xmlValue(node)
    gsub(',', '', x)
  }
  ex2 <- htmltab(doc = url, which = xp, bodyFun = popFun)

  expect_that(ex2[1,7], equals("1262"))
})


test_that("Example 3 works", {

  doc <- "http://en.wikipedia.org/wiki/FC_Bayern_Munich"
  xp2 <- "//td[text() = 'Head coach']/ancestor::table"
  ex3 <- htmltab(doc = doc, which = xp2, header = 0, encoding = "UTF-8", colNames = c("name", "role"))

  expect_that(ex3[1,1], equals("Pep Guardiola"))
  expect_that(colnames(ex3)[1], equals("name"))
})


test_that("Example 4 works", {

  library(XML)

  doc <- "http://en.wikipedia.org/wiki/Usage_share_of_web_browsers"
  xp3 <-  "//table[6]"
  bFun <- function(node) {
    x <- xmlValue(node)
    x <- gsub('%$', '', x)
    ifelse(x == '', NA, x)
  }
  ex4 <- htmltab(doc = doc, which = xp3, bodyFun = bFun)

  expect_that(ex4[1,1], equals("July 2013"))
  expect_that(ex4[1,2], equals("20.27"))
  expect_that(is.na(ex4[11,5]), is_true())
})
