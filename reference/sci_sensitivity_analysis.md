# SCI Sensitivity Analysis

Evaluates the stability of the Submission Confidence Index under
alternative pillar weight scenarios.

## Usage

``` r
sci_sensitivity_analysis(evidence, weight_grid)
```

## Arguments

- evidence:

  A validated evidence data.frame.

- weight_grid:

  A data.frame where each row is a weight scenario. Column names must
  match pillar names (`quality`, `trace`, `risk`, `usability`). Each row
  must sum to 1.

## Value

A tibble with one row per scenario, containing: `scenario` (row number),
the weight columns, `SCI`, and `band`.

## Examples

``` r
if (FALSE) { # \dontrun{
grid <- data.frame(
  quality   = c(0.4, 0.3, 0.25),
  trace     = c(0.2, 0.3, 0.25),
  risk      = c(0.3, 0.2, 0.25),
  usability = c(0.1, 0.2, 0.25)
)
sci_sensitivity_analysis(evidence, grid)
} # }
```
