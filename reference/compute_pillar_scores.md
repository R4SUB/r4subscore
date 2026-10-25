# Compute Pillar Scores

Aggregates indicator scores into pillar-level scores (one per domain).
Each pillar score is the mean of its indicator scores.

## Usage

``` r
compute_pillar_scores(evidence, config = sci_config_default())
```

## Arguments

- evidence:

  A validated evidence data.frame.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

## Value

A tibble with columns: `pillar`, `pillar_score`, `n_indicators`,
`weight`. The number of open critical findings (severity `critical`,
result `fail`) is attached as an `n_critical` attribute, which
[`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md)
reads to apply the non-compensatory gate.

## Examples

``` r
if (FALSE) { # \dontrun{
ps <- compute_pillar_scores(evidence)
ps
} # }
```
