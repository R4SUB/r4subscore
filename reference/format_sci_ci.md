# Format an SCI Confidence Interval

Produces the compact `SCI = 82.0 [76.0, 88.0] (95% CI)` form for
reports.

## Usage

``` r
format_sci_ci(x, ...)
```

## Arguments

- x:

  An `sci_ci` object from
  [`sci_confidence_interval()`](https://r4sub.github.io/r4subscore/reference/sci_confidence_interval.md).

- ...:

  Ignored.

## Value

A single character string.

## Examples

``` r
if (FALSE) { # \dontrun{
format_sci_ci(sci_confidence_interval(evidence, seed = 1))
} # }
```
