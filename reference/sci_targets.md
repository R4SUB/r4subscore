# Define an SCI Target

Records the readiness goal a submission is aiming for: an overall SCI
target and, optionally, a minimum score for individual pillars. Pair it
with
[`sci_gap_to_target()`](https://r4sub.github.io/r4subscore/reference/sci_gap_to_target.md)
to measure how far a scored run is from filing.

## Usage

``` r
sci_targets(
  target = "ready",
  pillar_targets = NULL,
  config = sci_config_default()
)
```

## Arguments

- target:

  Either a band name (for example `"ready"`), in which case the target
  SCI is that band's lower bound, or a single numeric SCI on the 0–100
  scale.

- pillar_targets:

  Optional named numeric vector of per-pillar minimum scores on the
  0–100 scale. Names must be among `"quality"`, `"trace"`, `"risk"`,
  `"usability"`.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md),
  used to resolve a band name to a numeric bound and to classify a
  numeric target.

## Value

A list of class `"sci_targets"` with `sci` (numeric target), `band` (the
band the target falls in), and `pillars` (named numeric vector of
per-pillar targets, possibly empty).

## Examples

``` r
sci_targets("ready")
#> ℹ SCI target: 85 (band "ready")
sci_targets(80, pillar_targets = c(risk = 75, trace = 85))
#> ℹ SCI target: 80 (band "minor_gaps")
#> ℹ   risk: >= 75
#> ℹ   trace: >= 85
```
