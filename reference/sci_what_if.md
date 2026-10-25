# What-If Analysis for a Single Indicator

Recomputes the pillar score and the SCI as if one indicator had a
different score, leaving every other indicator untouched. This gives the
exact SCI movement a remediation would produce, including the effect of
weight redistribution when pillars are missing.

## Usage

``` r
sci_what_if(evidence, indicator_id, new_score, config = sci_config_default())
```

## Arguments

- evidence:

  A validated evidence data.frame.

- indicator_id:

  The `indicator_id` to adjust.

- new_score:

  The hypothetical indicator score, a number in `[0, 1]`.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

## Value

A list with `indicator_id`, `indicator_domain`, `old_indicator_score`,
`new_indicator_score`, `old_pillar_score`, `new_pillar_score`,
`old_SCI`, `new_SCI`, `delta_SCI`, `old_band`, and `new_band`.

## Examples

``` r
if (FALSE) { # \dontrun{
sci_what_if(evidence, "Q1", new_score = 1)
} # }
```
