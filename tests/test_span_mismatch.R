context("Last column mismatch")

test_that("correct input checks", {
  url <- "German_federal_election,_2009.html"
  htmltable(doc = url, which = "/html/body/div[3]/div[2]/div[4]/table[3]", header = 1:2, body = "tr[position() > 2]") #error, last column span wrong
  expect_error(check_type(doc = "http://http://cran.at.r-project.org/", num = factor(4)))
})
