# Capture an SCI Snapshot

Readiness is a trajectory, not a single moment. A snapshot records the
SCI, the band, and the underlying pillar and indicator scores at one
point in time (a dry run, a database lock) under a label, so later runs
can be compared with
[`sci_diff()`](https://r4sub.github.io/r4subscore/reference/sci_diff.md)
and tracked with
[`sci_snapshot_history()`](https://r4sub.github.io/r4subscore/reference/sci_snapshot_history.md).

## Usage

``` r
sci_snapshot(
  evidence,
  label,
  config = sci_config_default(),
  run_context = NULL
)

# S3 method for class 'sci_snapshot'
print(x, ...)
```

## Arguments

- evidence:

  A validated evidence data.frame for this run.

- label:

  A short label for the run, for example `"DB lock minus 8 weeks"`.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

- run_context:

  Optional
  [r4subcore::r4sub_run_context](https://r4sub.github.io/r4subcore/reference/r4sub_run_context.html)
  to record alongside the scores.

- x:

  An `sci_snapshot`.

- ...:

  Ignored.

## Value

An object of class `"sci_snapshot"`.

## See also

[`sci_diff()`](https://r4sub.github.io/r4subscore/reference/sci_diff.md),
[`sci_snapshot_history()`](https://r4sub.github.io/r4subscore/reference/sci_snapshot_history.md).

## Examples

``` r
ctx <- suppressMessages(r4subcore::r4sub_run_context("S1", "DEV"))
ev <- suppressMessages(r4subcore::as_evidence(
  data.frame(
    asset_type = "dataset", asset_id = "ADSL", source_name = "p21",
    indicator_id = c("Q1", "T1"), indicator_name = c("Q1", "T1"),
    indicator_domain = c("quality", "trace"),
    severity = c("low", "high"), result = c("pass", "fail"),
    stringsAsFactors = FALSE
  ),
  ctx = ctx
))
snap <- sci_snapshot(ev, "dry run 1")
snap
#> ℹ SCI snapshot: "dry run 1"
#> ℹ   SCI 43.8 (high_risk)
```
