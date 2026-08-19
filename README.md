# compIndexBuilder

`compIndexBuilder` provides an interactive Shiny application for constructing
and analysing composite indices.

## Launch

```r
library(compIndexBuilder)
compIndexBuilder()
```

Optional `shiny::runApp()` arguments can be supplied directly, for example:

```r
compIndexBuilder(launch.browser = TRUE)
```

Version 2.0.0 adds multi-sheet Excel workbook handling, active-sheet refresh,
per-sheet downloads, improved data processing, forecasting, pillar-based
sub-indices, and statistical diagnostics.
