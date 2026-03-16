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
| `conditional` | 50-69     | Conditional — significant work needed |
| `high_risk`   | 0-49      | High risk — submission not advised    |

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
