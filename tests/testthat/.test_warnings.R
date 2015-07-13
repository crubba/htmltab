context("Correct warning")
 test_that("Correctly expanded", {
   url <- "http://www.tradingeconomics.com/netherlands/government-debt-to-gdp"
   expect_that(htmltab(doc = url), gives_warning("Argument 'which' was left unspecified. Choosing first table."))
   })

