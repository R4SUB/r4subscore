# Per-Indicator Contribution Breakdown

Breaks a pillar score down into the contribution of each indicator,
ranks indicators by how much score could be recovered by fixing them,
and attaches a short remediation hint for each one. Use this to answer
"what do we fix first?" once
[`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.md)
has answered the go/no-go question.

## Usage

``` r
indicator_contributions(evidence, config = sci_config_default())
```

## Arguments

- evidence:

  A validated evidence data.frame (from `r4subcore`).

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

## Value

A tibble sorted by `sci_headroom` descending, with columns:
`indicator_id`, `indicator_name`, `indicator_domain`, `indicator_score`,
`n_in_pillar`, `pillar_contribution`, `pillar_headroom`, `sci_headroom`,
`worst_result`, `suggestion`.

## Details

A pillar score is the mean of its indicator scores (missing indicators
are dropped), so within a pillar with `k` scored indicators each
indicator contributes `indicator_score / k` to the pillar score.
Bringing one indicator up to a perfect 1 would raise the pillar score by
`(1 - indicator_score) / k`; that quantity is the `pillar_headroom`.
Multiplying it by the pillar weight gives `sci_headroom`, an approximate
cap on how many SCI points a single fix can buy. Indicators are returned
in `sci_headroom` order so the highest-value fixes come first.

The `sci_headroom` figure uses the nominal pillar weight from `config`.
When a whole pillar is missing its weight is redistributed across the
remaining pillars, so the exact SCI movement from a fix can differ
slightly. Use
[`sci_what_if()`](https://r4sub.github.io/r4subscore/reference/sci_what_if.md)
for an exact recomputation.

## Examples

``` r
if (FALSE) { # \dontrun{
contrib <- indicator_contributions(evidence)
head(contrib)
} # }
```
