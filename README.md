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

## Recommended data format for repeated indicator-year observations

Version 2.1.0 accepts wide spreadsheets in which the indicator and year are both
kept in the column name. For example:

| Country | IN1-2019 | IN1-2020 | IN2-2019 | IN2-2020 | IN3-2019 | IN3-2020 |
|---|---:|---:|---:|---:|---:|---:|
| A | 12.1 | 13.0 | 4.2 | 4.5 | 18.0 | 17.0 |
| B | 10.4 | 11.2 | 3.9 | 4.1 | 20.0 | 19.3 |

With **Data layout = Auto-detect**, the app reshapes this internally to:

| Country | Year | IN1 | IN2 | IN3 |
|---|---:|---:|---:|---:|
| A | 2019 | 12.1 | 4.2 | 18.0 |
| A | 2020 | 13.0 | 4.5 | 17.0 |
| B | 2019 | 10.4 | 3.9 | 20.0 |
| B | 2020 | 11.2 | 4.1 | 19.3 |

Do **not** rename all columns to years only. Headers such as `IN1-2019` are
preferred because they preserve both the sub-indicator identity and the time
period. Common variants such as `IN1_2019`, `IN1.2019`, and `IN1 2019` are also
recognised.

## Missing values

Text codes such as `#N/A`, `N/A`, `NA`, `..`, `...`, and `NULL` can be
standardised to missing values during import. Numeric zero is **not** treated as
missing by default because zero may be a legitimate observation. If a source
uses `0` or `0.00` specifically to mean "no data", enable **Treat numeric 0 /
0.00 as missing** before reloading the sheet.

After import, missing observations can be removed, retained with available
weights re-normalised, median-imputed, interpolated, or imputed with
`missForest`.

## Indicator direction

For mixed directions, choose **Mixed** under Indicator direction. An indicator
for which a high value is undesirable (for example `IN3`) should be set to
**Lower is better**. After indicator-year reshaping, this setting is applied to
the indicator itself across all years.

## Other features

Version 2.1.0 retains the Version 2 multi-sheet Excel workflow, per-sheet and
workbook-wide downloads, normalisation, equal/custom weighting, rankings,
time-series analysis and forecasting, entity comparisons, pillar/sub-index
construction, PCA, reliability diagnostics, sensitivity analysis, correlation
heatmaps, and weighted flow visualisations.
