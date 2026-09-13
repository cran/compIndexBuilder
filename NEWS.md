# compIndexBuilder 2.1.0

## Data-format and missing-data update

* Added automatic recognition of wide indicator-year headers such as `IN1-2019`,
  `IN1-2020`, `IN2-2019`, and `IN2-2020`.
* Added automatic reshaping of that wide layout into panel form with one time
  column (`Year`) and one column per indicator (`IN1`, `IN2`, ...), preserving
  the distinction between indicators and years.
* Added a selectable data-layout mode: Auto-detect, Standard/already tidy, or
  Force indicator-year wide format.
* Added configurable text missing-value codes. Common codes such as `#N/A`,
  `N/A`, `NA`, `..`, `...`, and `NULL` can now be standardised to `NA` before
  numeric conversion.
* Added an explicit zero-as-missing option. Numeric `0` / `0.00` is preserved
  by default because zero may be a valid observation, and is converted to
  missing only when the user opts in.
* Added an import-guidance panel showing whether indicator-year columns were
  detected/reshaped, which years and indicators were recognised, and how many
  missing codes or zero values were converted.
* Preserved meaningful source headers instead of converting punctuation in
  names such as `IN1-2019` before layout detection.
* Clarified the mixed-direction workflow: indicators for which high values are
  undesirable can be set once to "Lower is better" after reshaping.

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
