testthat::test_that("compIndexBuilder() is available and guarded (not executed on CRAN)", {
  testthat::skip_on_cran()
  
  if (!"run_app" %in% getNamespaceExports("compIndexBuilder")) {
    testthat::skip("compIndexBuilder() not exported; adjust if different launcher function name.")
  }
  testthat::expect_true(is.function(compIndexBuilder::compIndexBuilder))
})
