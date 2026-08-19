# compIndexBuilder 2.0.0

## Major update

* Added workbook-wide multi-sheet Excel support.
* Added an active-sheet selector that lists all sheets in an uploaded workbook.
* Added controls to refresh the workbook sheet list and reload the active sheet.
* Added per-sheet CSV downloads and ZIP export of all workbook sheets.
* Unified CSV and Excel data-loading logic and removed duplicated server paths.
* Improved missing-data handling so missing indicators are not silently treated as zero.
* Added median, interpolation, and missForest-based imputation options.
* Added min-max and z-score normalisation and mixed indicator directions.
* Improved entity-level rankings for panel data and weight-impact comparisons.
* Improved single- and multi-entity time-series forecasting.
* Corrected time-period filtering in entity comparisons.
* Expanded pillar/sub-index construction with equal, custom, correlation-based,
  and PCA-based weighting.
* Added diagnostic outputs for Cronbach's alpha, coefficient of variation, PCA,
  sensitivity analysis, correlation heatmaps, and Sankey diagrams.
* Added processed-data export.
