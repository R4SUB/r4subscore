# Submission Confidence Index (SCI) Scoring

The `r4subscore` package computes the **Submission Confidence Index
(SCI)**, a composite 0-100 score summarising submission readiness across
four pillars: quality, trace, risk, and usability.

``` r

library(r4subscore)
```

## Default configuration

[`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md)
returns the pillar weights and decision-band thresholds:

``` r

cfg <- sci_config_default()
cfg$pillar_weights
#>   quality     trace      risk usability 
#>      0.35      0.25      0.25      0.15
cfg$bands
#> $ready
#> [1]  85 100
#> 
#> $minor_gaps
#> [1] 70 84
#> 
#> $conditional
#> [1] 50 69
#> 
#> $high_risk
#> [1]  0 49
```

The default weights are: quality 35%, trace 25%, risk 25%, usability
15%.

## Decision bands

| Band          | SCI range | Meaning                               |
|---------------|-----------|---------------------------------------|
| `ready`       | 85-100    | Submission ready                      |
| `minor_gaps`  | 70-84     | Minor gaps to address                 |
| `conditional` | 50-69     | Conditional - significant work needed |
| `high_risk`   | 0-49      | High risk - submission not advised    |

## Building example evidence

``` r

ev <- data.frame(
  run_id           = "run-001",
  study_id         = "STUDY01",
  asset_type       = "dataset",
  asset_id         = rep(c("ADSL", "ADAE"), each = 6),
  source_name      = "r4subcore",
  source_version   = "0.1.2",
  indicator_id     = paste0("X-00", 1:12),
  indicator_name   = paste0("Indicator ", 1:12),
  indicator_domain = rep(c("quality", "trace", "risk", "usability",
                           "quality", "trace"), 2),
  severity         = "info",
  result           = c("pass", "pass", "pass", "pass", "pass", "pass",
                       "warn", "pass", "fail", "pass", "pass", "warn"),
  metric_value     = c(1, 1, 1, 1, 1, 1, 0.8, 1, 0, 1, 1, 0.9),
  metric_unit      = "score",
  message          = paste0("Indicator ", 1:12, " result"),
  location         = rep(c("ADSL", "ADAE"), each = 6),
  evidence_payload = "{}",
  created_at       = Sys.time(),
  stringsAsFactors = FALSE
)
```

## Computing pillar scores

[`compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.md)
aggregates the mean metric value per domain:

``` r

ps <- compute_pillar_scores(ev)
ps
#> # A tibble: 4 × 4
#>   pillar    pillar_score n_indicators weight
#>   <chr>            <dbl>        <int>  <dbl>
#> 1 quality          0.875            4   0.35
#> 2 trace            0.875            4   0.25
#> 3 risk             0.5              2   0.25
#> 4 usability        1                2   0.15
```

## Computing the SCI

``` r

result <- compute_sci(ps)
result$SCI
#> [1] 80
result$band
#> [1] "minor_gaps"
print(result)
#> ℹ Submission Confidence Index: 80
#> ℹ Decision Band: "minor_gaps"
#> ℹ   quality: 87.5 (weight: 0.35)
#> ℹ   trace: 87.5 (weight: 0.25)
#> ℹ   risk: 50 (weight: 0.25)
#> ℹ   usability: 100 (weight: 0.15)
```

## Detailed explanation

[`sci_explain()`](https://r4sub.github.io/r4subscore/reference/sci_explain.md)
returns a breakdown showing each pillar’s contribution to the final SCI
and any score loss:

``` r

expl <- sci_explain(ev)
expl$pillar_contributions
#> # A tibble: 4 × 6
#>   pillar    pillar_score n_indicators weight contribution  loss
#>   <chr>            <dbl>        <int>  <dbl>        <dbl> <dbl>
#> 1 quality          0.875            4   0.35         30.6   4.4
#> 2 trace            0.875            4   0.25         21.9   3.1
#> 3 risk             0.5              2   0.25         12.5  12.5
#> 4 usability        1                2   0.15         15     0
```

## Sensitivity analysis

[`sci_sensitivity_analysis()`](https://r4sub.github.io/r4subscore/reference/sci_sensitivity_analysis.md)
evaluates the SCI across a grid of alternative pillar weights, useful
for understanding how robust the score is to weighting choices:

``` r

grid <- data.frame(
  quality    = c(0.35, 0.50, 0.25),
  trace      = c(0.25, 0.20, 0.30),
  risk       = c(0.25, 0.20, 0.25),
  usability  = c(0.15, 0.10, 0.20)
)

sa <- sci_sensitivity_analysis(ev, weight_grid = grid)
sa
#> # A tibble: 3 × 7
#>   scenario quality trace  risk usability   SCI band      
#>      <int>   <dbl> <dbl> <dbl>     <dbl> <dbl> <chr>     
#> 1        1    0.35  0.25  0.25      0.15  80   minor_gaps
#> 2        2    0.5   0.2   0.2       0.1   81.2 minor_gaps
#> 3        3    0.25  0.3   0.25      0.2   80.6 minor_gaps
```

## Custom weights

Pass a custom config to favour particular pillars:

``` r

cfg_custom <- sci_config_default(
  pillar_weights = c(quality = 0.50, trace = 0.20,
                     risk = 0.20, usability = 0.10)
)
ps2   <- compute_pillar_scores(ev, config = cfg_custom)
sci2  <- compute_sci(ps2, config = cfg_custom)
sci2$SCI
#> [1] 81.2
sci2$band
#> [1] "minor_gaps"
```

## Confidence intervals

A single SCI number hides how much of the score is measurement noise. An
SCI of 72 could be a solid `conditional` result or a borderline
`minor_gaps` one that happened to round down.
[`sci_confidence_interval()`](https://r4sub.github.io/r4subscore/reference/sci_confidence_interval.md)
resamples the evidence and reports where the SCI is likely to sit:

``` r

ci <- sci_confidence_interval(ev, n_boot = 500, seed = 1)
ci
#> ℹ SCI = 80.0 [61.2, 96.9] (95% CI)
#> ℹ Point band: "minor_gaps"
#> ! Interval spans bands "conditional" to "ready".
```

[`format_sci_ci()`](https://r4sub.github.io/r4subscore/reference/format_sci_ci.md)
gives the compact form for a report or a slide:

``` r

format_sci_ci(ci)
#> [1] "SCI = 80.0 [61.2, 96.9] (95% CI)"
```

Read the interval, not just the point estimate. When the interval sits
entirely inside one decision band, the band is a firm call. When it
straddles two bands, as flagged in the printout, the readiness decision
is sensitive to which checks were run, and the honest answer is that
more evidence is needed before the band can be trusted. The default
scheme resamples indicator scores within each pillar; pass
`by = "evidence"` to resample the raw evidence rows instead.

## Critical findings cap the band

The SCI is a weighted average, so a strong pillar can lift the score
even when another pillar hides a serious problem. That is the wrong
behavior for a readiness call: a single critical failure should not be
averaged away. The scoring config carries a non-compensatory gate that
caps the band when an open critical finding is present. The pipeline
applies it automatically, counting critical findings (severity
`critical`, result `fail`) from the evidence, but it is easiest to see
with the count passed directly:

``` r

strong <- data.frame(
  pillar       = c("quality", "trace", "risk", "usability"),
  pillar_score = c(0.95, 0.92, 0.90, 0.93),
  weight       = c(0.35, 0.25, 0.25, 0.15)
)

compute_sci(strong, n_critical = 0)$band
#> [1] "ready"

capped <- compute_sci(strong, n_critical = 1)
capped$band
#> [1] "conditional"
capped$gate_reason
#> [1] "1 open critical finding; band capped at conditional."
```

The numeric SCI is identical in both cases; only the band moves, so the
score still tells you how much work remains. A sponsor that wants a
different policy can move the cap or turn the gate off through
`sci_config_default(gate = ...)`.

## Tracking readiness over time

A single score answers “are we ready today”. The question a team
actually lives with is “are we on track to file”. Snapshots record the
score at each dry run so successive runs can be compared. Here two runs
on the same study, with one traceability failure fixed between them:

``` r

ctx <- r4subcore::r4sub_run_context("STUDY1", "DEV")
#> ℹ Run context created: "R4S-20261025205033-f2pi2o1y"

run_evidence <- function(trace_result) {
  r4subcore::as_evidence(
    data.frame(
      asset_type = "dataset", asset_id = "ADSL", source_name = "pinnacle21",
      indicator_id = c("Q-LABEL", "T-TRACE", "R-RPN"),
      indicator_name = c("Labels present", "ADaM traces to SDTM", "Risk in range"),
      indicator_domain = c("quality", "trace", "risk"),
      severity = c("low", "high", "medium"),
      result = c("pass", trace_result, "pass"),
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  )
}

week1 <- sci_snapshot(run_evidence("fail"), "DB lock minus 8 weeks")
#> ✔ Evidence table created: 3 rows
week2 <- sci_snapshot(run_evidence("pass"), "DB lock minus 4 weeks")
#> ✔ Evidence table created: 3 rows

week1
#> ℹ SCI snapshot: "DB lock minus 8 weeks"
#> ℹ   SCI 45.6 (high_risk)
week2
#> ℹ SCI snapshot: "DB lock minus 4 weeks"
#> ℹ   SCI 52.9 (conditional)
```

[`sci_diff()`](https://r4sub.github.io/r4subscore/reference/sci_diff.md)
reports what moved and why:

``` r

d <- sci_diff(week1, week2)
d
#> ℹ SCI change: 7.3 (up)
#> ℹ Band: high_risk -> conditional
#> ℹ Indicators moved: 1
d$indicators[, c("indicator_id", "status", "delta")]
#> # A tibble: 3 × 3
#>   indicator_id status    delta
#>   <chr>        <chr>     <dbl>
#> 1 T-TRACE      improved   0.25
#> 2 Q-LABEL      unchanged  0   
#> 3 R-RPN        unchanged  0
```

[`sci_snapshot_history()`](https://r4sub.github.io/r4subscore/reference/sci_snapshot_history.md)
binds a series into one row per run, ready for a trend plot:

``` r

sci_snapshot_history(list(week1, week2))
#> # A tibble: 2 × 8
#>   label                   sci band      n_critical quality trace  risk usability
#>   <chr>                 <dbl> <chr>          <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1 DB lock minus 8 weeks  45.6 high_risk          0    0.75  0      0.5        NA
#> 2 DB lock minus 4 weeks  52.9 conditio…          0    0.75  0.25   0.5        NA
```
