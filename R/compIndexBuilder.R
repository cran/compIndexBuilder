#' Launch the Composite Index Builder Shiny Application
#'
#' Launches the interactive Shiny application bundled with
#' \pkg{compIndexBuilder}. The application supports CSV and multi-sheet Excel
#' workbooks, composite-index construction, weighting, diagnostics, time-series
#' analysis, forecasting, comparisons, and pillar-based sub-indices.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], such as
#'   `launch.browser`, `host`, or `port`.
#'
#' @return Invisibly returns the value returned by [shiny::runApp()]. The
#'   function is primarily called for its side effect of launching the Shiny
#'   application.
#'
#' @examples
#' if (interactive()) {
#'   compIndexBuilder()
#' }
#'
#' @export
compIndexBuilder <- function(...) {
  app_dir <- system.file("shiny-app", package = "compIndexBuilder")

  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    stop("The bundled compIndexBuilder Shiny application could not be found.",
         call. = FALSE)
  }

  shiny::runApp(appDir = app_dir, ...)
}
