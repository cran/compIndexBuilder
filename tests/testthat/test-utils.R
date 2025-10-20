testthat::test_that("basic utility functions (if present) behave on small input", {
  # Add lightweight checks for simple utilities if they exist, otherwise skip
  ns <- asNamespace("compIndexBuilder")
  if (!exists("scale_min_max", where = ns, inherits = FALSE)) {
    testthat::skip("scale_min_max() not found; this test is optional.")
  }
  f <- get("scale_min_max", envir = ns)
  v <- c(1, 2, 3)
  out <- f(v)
  testthat::expect_equal(min(out, na.rm = TRUE), 0)
  testthat::expect_equal(max(out, na.rm = TRUE), 1)
})
