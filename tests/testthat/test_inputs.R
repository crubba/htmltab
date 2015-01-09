context("Input checks")

test_that("Prompts errors correctly", {
  expect_error(check_type(doc = "http://http://cran.at.r-project.org/", which = factor(4)))
})

test_that("check_type produces class output", {

  x <- check_type(doc = "http://en.wikipedia.org/wiki/Indian_general_election,_2014", which = "//table[5]")
  expect_that(x, is_a("XMLInternalDocument"))

  y <- check_type(doc = "http://en.wikipedia.org/wiki/Indian_general_election,_2014", which = NULL)
  expect_that(y, is_a("XMLInternalDocument"))

  parsed <- XML::htmlParse("http://en.wikipedia.org/wiki/Indian_general_election,_2014")
  z <- check_type(doc = parsed, which = 3)
  expect_that(z, is_a("XMLInternalDocument"))
})
