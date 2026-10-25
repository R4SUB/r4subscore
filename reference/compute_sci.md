# Compute Submission Confidence Index (SCI)

Computes the SCI from pillar scores as a weighted sum scaled to 0–100,
with decision band classification.

## Usage

``` r
compute_sci(pillar_scores, config = sci_config_default(), n_critical = NULL)
```

## Arguments

- pillar_scores:

  A tibble from
  [`compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.md)
  with columns `pillar`, `pillar_score`, `weight`.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

- n_critical:

  Number of open critical findings. When `NULL` (the default), it is
  read from the `n_critical` attribute that
  [`compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.md)
  attaches, so the gate applies automatically through the standard
  pipeline. Pass a value to override.

## Value

A list of class `"sci_result"` with:

- `SCI`: numeric 0–100

- `band`: character band classification, after any gate cap

- `pillar_scores`: the input pillar scores tibble

- `weights_used`: named numeric vector of effective weights

- `n_critical`: number of open critical findings considered

- `gated`: `TRUE` when a critical finding capped the band

- `gate_reason`: a short explanation when `gated`, otherwise `NA`

## Details

The SCI is computed as:

`SCI = round(sum(pillar_score * weight) * 100, 1)`

Pillars with `NA` scores are excluded from both the numerator and the
weight normalization denominator.

The score is a weighted average, which on its own lets a strong pillar
offset a weak one. The gate in `config$gate` adds a non-compensatory
rule on top: an open critical finding caps the reported band (by default
at `conditional`), so a critical failure cannot be washed out by good
scores elsewhere. The numeric SCI is left untouched; only the band is
capped, and the reason is recorded.

## Examples

``` r
if (FALSE) { # \dontrun{
ps <- compute_pillar_scores(evidence)
result <- compute_sci(ps)
result$SCI
result$band
} # }
```
