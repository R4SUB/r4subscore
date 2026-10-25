# Check that an SCI Degradation Curve Never Rises

Verifies that a degradation curve is monotone non-increasing: injecting
more failures must never improve the Submission Confidence Index. A
rising step would signal a defect in the scoring model.

## Usage

``` r
sci_monotone_check(curve, tol = 1e-08)
```

## Arguments

- curve:

  A tibble from
  [`sci_degradation_curve()`](https://r4sub.github.io/r4subscore/reference/sci_degradation_curve.md),
  or any data.frame with `step` and `SCI` columns.

- tol:

  Numeric tolerance for treating a small rise as noise. Defaults to
  `1e-8`.

## Value

A list with:

- `monotone`: `TRUE` if no step rises beyond `tol`

- `max_increase`: the largest step-to-step increase (0 if none)

- `violations`: a tibble of the steps that rose, empty if none

## Examples

``` r
if (FALSE) { # \dontrun{
curve <- sci_degradation_curve(evidence)
sci_monotone_check(curve)$monotone
} # }
```
