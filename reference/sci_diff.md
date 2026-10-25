# Diff Two SCI Snapshots

Reports what moved between two snapshots: the overall SCI change, any
band change, per-pillar deltas, and which indicators improved,
regressed, were added, or were removed. This answers "what changed and
why since the last run", which is the question a readiness review
actually asks.

## Usage

``` r
sci_diff(before, after)

# S3 method for class 'sci_diff'
print(x, ...)
```

## Arguments

- before, after:

  `sci_snapshot` objects from
  [`sci_snapshot()`](https://r4sub.github.io/r4subscore/reference/sci_snapshot.md),
  in chronological order.

- x:

  An `sci_diff`.

- ...:

  Ignored.

## Value

An object of class `"sci_diff"` with `sci_delta`, `band_from`,
`band_to`, `band_changed`, a `pillars` tibble of per-pillar deltas, and
an `indicators` tibble of per-indicator changes ordered by the size of
the move.

## See also

[`sci_snapshot()`](https://r4sub.github.io/r4subscore/reference/sci_snapshot.md).

## Examples

``` r
ctx <- suppressMessages(r4subcore::r4sub_run_context("S1", "DEV"))
mk <- function(res) suppressMessages(r4subcore::as_evidence(
  data.frame(
    asset_type = "dataset", asset_id = "ADSL", source_name = "p21",
    indicator_id = c("Q1", "T1"), indicator_name = c("Q1", "T1"),
    indicator_domain = c("quality", "trace"),
    severity = c("low", "high"), result = res,
    stringsAsFactors = FALSE
  ),
  ctx = ctx
))
before <- sci_snapshot(mk(c("pass", "fail")), "before")
after  <- sci_snapshot(mk(c("pass", "pass")), "after")
sci_diff(before, after)
#> ℹ SCI change: 10.4 (up)
#> ℹ Band: high_risk -> conditional
#> ℹ Indicators moved: 1
```
