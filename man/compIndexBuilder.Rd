\name{compIndexBuilder}
\alias{compIndexBuilder}
\title{Launch the Composite Index Builder Shiny Application}
\usage{
compIndexBuilder(...)
}
\arguments{
  \item{...}{Additional arguments passed to \code{shiny::runApp()}, such as
  \code{launch.browser}, \code{host}, or \code{port}.}
}
\description{
Launches the interactive Shiny application bundled with
\pkg{compIndexBuilder}. The application supports CSV and multi-sheet Excel
workbooks, composite-index construction, weighting, diagnostics, time-series
analysis, forecasting, comparisons, and pillar-based sub-indices.
}
\value{
Invisibly returns the value returned by \code{shiny::runApp()}. The function
is primarily called for its side effect of launching the Shiny application.
}
\examples{
if (interactive()) {
  compIndexBuilder()
}
}
