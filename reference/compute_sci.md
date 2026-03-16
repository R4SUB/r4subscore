# Compute Submission Confidence Index (SCI)

Computes the SCI from pillar scores as a weighted sum scaled to 0–100,
with decision band classification.

## Usage

``` r
compute_sci(pillar_scores, config = sci_config_default())
```

## Arguments

- pillar_scores:

  A tibble from
  [`compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.md)
  with columns `pillar`, `pillar_score`, `weight`.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

## Value

A list of class `"sci_result"` with:

- `SCI`: numeric 0–100

- `band`: character band classification

- `pillar_scores`: the input pillar scores tibble

- `weights_used`: named numeric vector of effective weights

## Details

The SCI is computed as:

`SCI = round(sum(pillar_score * weight) * 100, 1)`

Pillars with `NA` scores are excluded from both the numerator and the
weight normalization denominator.

## Examples

``` r
if (FALSE) { # \dontrun{
ps <- compute_pillar_scores(evidence)
result <- compute_sci(ps)
result$SCI
result$band
} # }
```
