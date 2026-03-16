# Compute Indicator-Level Scores

Converts each indicator in an evidence table into a numeric score (0–1)
using severity-weighted result scoring.

## Usage

``` r
compute_indicator_scores(evidence)
```

## Arguments

- evidence:

  A validated evidence data.frame (from `r4subcore`).

## Value

A tibble with columns: `indicator_id`, `indicator_name`,
`indicator_domain`, `n_evidence`, `indicator_score`.

## Details

For each evidence row:

- `result_score` = `r4subcore::result_to_score(result)` (pass=1,
  warn=0.5, fail=0)

- `severity_weight` = `r4subcore::severity_to_weight(severity)` (info=0,
  ..., critical=1)

- `weighted_score` = `result_score * (1 - severity_weight)`

Rows are grouped by `indicator_id` and `indicator_domain`, and the
indicator score is the mean of `weighted_score` within each group.

## Examples

``` r
if (FALSE) { # \dontrun{
scores <- compute_indicator_scores(evidence)
scores
} # }
```
