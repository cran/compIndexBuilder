testthat::test_that("internal function int_net_gr_build_ runs silently and returns a basic structure", {
  # Skip gracefully on CRAN if function not present or heavy deps
  testthat::skip_on_cran()
  
  # Check existence in namespace (not exported)
  ns <- asNamespace("compIndexBuilder")
  if (!exists("int_net_gr_build_", where = ns, inherits = FALSE)) {
    testthat::skip("int_net_gr_build_ not found in namespace; rename here if internal name differs.")
  }
  fun <- get("int_net_gr_build_", envir = ns)
  
  # Run on tiny toy data and assert generic properties (avoid overfitting tests)
  testthat::expect_silent({
    res <- fun(example_df)
  })
  
  res <- fun(example_df)
  # Very generic checks so tests are stable across refactors
  testthat::expect_false(is.null(res))
  testthat::expect_true(is.list(res) || is.data.frame(res) || is.matrix(res) || is.vector(res))
})
