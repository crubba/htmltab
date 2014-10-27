context("Input checks")

test_that("Prompts errors correctly", {
  expect_error(check_type(doc = "http://http://cran.at.r-project.org/", which = factor(4)))
})

test_that("check_type produces list ouput", {

  expect_true(is.list(check_type(doc = "http://en.wikipedia.org/wiki/Indian_general_election,_2014", which = "//table[5]")))

  expect_true(is.list(check_type(doc = "http://en.wikipedia.org/wiki/Indian_general_election,_2014", which = NULL)))

  parsed <- htmlParse("http://en.wikipedia.org/wiki/Indian_general_election,_2014")
  expect_true(is.list(check_type(doc = parsed, which = 3)))

})
