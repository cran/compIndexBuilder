test_that("bundled Shiny app is installed", {
  app_dir <- system.file("shiny-app", package = "compIndexBuilder")
  expect_true(nzchar(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
})

test_that("launcher is exported as a function", {
  expect_true(is.function(compIndexBuilder))
})
