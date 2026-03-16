# Explain SCI Contributors

Identifies which indicators contribute most to SCI loss and provides a
breakdown of pillar contributions.

## Usage

``` r
sci_explain(evidence, config = sci_config_default())
```

## Arguments

- evidence:

  A validated evidence data.frame.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

## Value

A list with:

- `indicator_contributions`: tibble of per-indicator loss contributions

- `pillar_contributions`: tibble of per-pillar contributions to SCI

## Details

For each indicator, the contribution to SCI loss is:

`loss = pillar_weight * (1 - indicator_score) / n_indicators_in_pillar`

This gives a sense of how much each indicator drags the SCI down.
Results are sorted by `loss` descending (worst contributors first).

## Examples

``` r
if (FALSE) { # \dontrun{
expl <- sci_explain(evidence)
expl$indicator_contributions
expl$pillar_contributions
} # }
```
