context("Input checks")

test_that("correct input checks", {
  #expect_error(check_type(doc = "nourl"), "Invalid 'doc' input: String is no URL")
  expect_error(check_type(doc = "http://http://cran.at.r-project.org/", num = factor(4)))
})
