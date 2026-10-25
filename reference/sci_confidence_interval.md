# Bootstrap Confidence Interval for the SCI

Turns the SCI point estimate into an interval by resampling the
underlying evidence. This answers the difference between "solidly
conditional" and "borderline ready depending on measurement noise" that
a single number cannot.

## Usage

``` r
sci_confidence_interval(
  evidence,
  config = sci_config_default(),
  level = 0.95,
  n_boot = 2000,
  by = c("indicator", "evidence"),
  seed = NULL
)
```

## Arguments

- evidence:

  A validated evidence data.frame (from `r4subcore`).

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

- level:

  Confidence level, a number in `(0, 1)`. Default `0.95`.

- n_boot:

  Number of bootstrap replicates. Default `2000`.

- by:

  Resampling scheme, `"indicator"` or `"evidence"`.

- seed:

  Optional integer seed for reproducibility.

## Value

A list of class `"sci_ci"` with `SCI` (point estimate), `lower`,
`upper`, `level`, `n_boot`, `by`, `band`, `lower_band`, `upper_band`,
and `replicates` (the bootstrap SCI vector).

## Details

Two resampling schemes are offered:

- `by = "indicator"` (default) resamples the indicator scores within
  each pillar with replacement, then recomputes the pillar mean and the
  SCI. This treats the indicators actually measured in a pillar as a
  sample from the checks that could have been run, and is stable when
  the raw evidence is uneven across pillars.

- `by = "evidence"` resamples the raw evidence rows within each pillar
  with replacement and runs the full scoring pipeline on each replicate.
  This propagates row-level noise but can drop whole indicators from a
  replicate.

A pillar with a single indicator carries no resampling variance, so a
narrow interval there is expected rather than a sign of high confidence.

## Examples

``` r
if (FALSE) { # \dontrun{
ci <- sci_confidence_interval(evidence, seed = 1)
ci
} # }
```
