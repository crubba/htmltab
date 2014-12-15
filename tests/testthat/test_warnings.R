context("Correct warning")

test_that("Correctly expanded", {

  url <- "http://www.tradingeconomics.com/netherlands/government-debt-to-gdp"
  expect_that(htmltable(doc = url), gives_warning("Argument 'which' left unspecified. Choosing first table."))
})

