\name{compIndexBuilder}
\alias{compIndexBuilder}
\title{Composite Index Builder Shiny Application}
\description{
The \pkg{'compIndexBuilder'} package provides a Shiny application and helper
functions to build composite indices from tabular indicator data.
}
\details{
This package includes modules for data preparation, weighting and
visualization. The Shiny interface is intended to be launched via
\code{\link{compIndexBuilder}} in an interactive R session.
}
\seealso{\code{\link{compIndexBuilder}}}
\value{
No return value, called for side effects.
The function launches a 'Shiny' web application for composite index construction.
}
\author{
Hossein Hassani and Leila Marvian Mashhad and Steve Macfeely and Petra Kynclova and Nour Barnat and Fernando CANTU BAZALDUA.
}
\examples{
if (interactive()) {
 toy_data <- data.frame(a = 1:5, b = 2:6)
 res <- compIndexBuilder(toy_data)
 summary(res)
}
}
