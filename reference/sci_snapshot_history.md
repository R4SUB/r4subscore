# Build a History Table from SCI Snapshots

Binds a list of snapshots into one tidy row per snapshot, with the SCI,
band, critical-finding count, and each pillar score in its own column,
ready for a trend plot.

## Usage

``` r
sci_snapshot_history(snapshots)
```

## Arguments

- snapshots:

  A list of `sci_snapshot` objects, in chronological order.

## Value

A tibble with one row per snapshot.

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
s1 <- sci_snapshot(mk(c("pass", "fail")), "week 1")
s2 <- sci_snapshot(mk(c("pass", "pass")), "week 2")
sci_snapshot_history(list(s1, s2))
#> # A tibble: 2 × 8
#>   label    sci band        n_critical quality trace  risk usability
#>   <chr>  <dbl> <chr>            <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1 week 1  43.8 high_risk            0    0.75  0       NA        NA
#> 2 week 2  54.2 conditional          0    0.75  0.25    NA        NA
```
