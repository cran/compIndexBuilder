load_data_prep_helpers <- function() {
  helper <- system.file("shiny-app", "data_prep_helpers.R", package = "compIndexBuilder")
  expect_true(nzchar(helper))
  expect_true(file.exists(helper))
  env <- new.env(parent = baseenv())
  sys.source(helper, envir = env)
  env
}

test_that("indicator-year wide data are detected and reshaped", {
  h <- load_data_prep_helpers()
  d <- data.frame(Country = c("A", "B"), check.names = FALSE)
  d[["IN1-2019"]] <- c(1, 2)
  d[["IN1-2020"]] <- c(3, 4)
  d[["IN2-2019"]] <- c(5, 6)
  d[["IN2-2020"]] <- c(7, 8)

  out <- h$prepare_imported_data(d, layout = "auto")

  expect_equal(names(out), c("Country", "Year", "IN1", "IN2"))
  expect_equal(nrow(out), 4L)
  expect_equal(out$Country, c("A", "A", "B", "B"))
  expect_equal(out$Year, c(2019L, 2020L, 2019L, 2020L))
  expect_equal(out$IN1, c(1, 3, 2, 4))
  expect_equal(out$IN2, c(5, 7, 6, 8))

  report <- attr(out, "compIndex_import_report")
  expect_true(report$reshaped)
  expect_equal(report$indicator_count, 2L)
  expect_equal(report$year_count, 2L)
})

test_that("common text missing codes are standardised before numeric conversion", {
  h <- load_data_prep_helpers()
  d <- data.frame(Country = c("A", "B", "C"), check.names = FALSE)
  d[["IN1-2019"]] <- c("1.5", "..", "3.5")
  d[["IN1-2020"]] <- c("#N/A", "2.5", "4.5")
  d[["IN2-2019"]] <- c("5", "6", "7")
  d[["IN2-2020"]] <- c("8", "9", "10")

  out <- h$prepare_imported_data(d, layout = "auto")

  expect_true(is.numeric(out$IN1))
  expect_true(is.na(out$IN1[2]))
  expect_true(is.na(out$IN1[3]))
  report <- attr(out, "compIndex_import_report")
  expect_gte(report$missing_token_replacements, 2L)
})

test_that("zero remains valid by default and can be explicitly treated as missing", {
  h <- load_data_prep_helpers()
  d <- data.frame(Entity = c("A", "B"), Value = c(0, 2), check.names = FALSE)

  keep_zero <- h$prepare_imported_data(d, layout = "standard", zero_is_missing = FALSE)
  drop_zero <- h$prepare_imported_data(d, layout = "standard", zero_is_missing = TRUE)

  expect_equal(keep_zero$Value[1], 0)
  expect_true(is.na(drop_zero$Value[1]))
  expect_equal(attr(drop_zero, "compIndex_import_report")$zero_replacements, 1L)
})

test_that("standard layout mode does not reshape even when headers match", {
  h <- load_data_prep_helpers()
  d <- data.frame(Entity = c("A", "B"), check.names = FALSE)
  d[["IN1-2019"]] <- c(1, 2)
  d[["IN1-2020"]] <- c(3, 4)
  d[["IN2-2019"]] <- c(5, 6)
  d[["IN2-2020"]] <- c(7, 8)

  out <- h$prepare_imported_data(d, layout = "standard")

  expect_true("IN1-2019" %in% names(out))
  expect_false("Year" %in% names(out))
  report <- attr(out, "compIndex_import_report")
  expect_true(report$indicator_year_detected)
  expect_false(report$reshaped)
})
