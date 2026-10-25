# SCI Degradation Curve

Progressively degrades an evidence table and records how the Submission
Confidence Index responds. The curve is the operational form of
construct validity: a well-behaved index must fall (never rise) as
passing checks are turned into failures.

## Usage

``` r
sci_degradation_curve(evidence, n_steps = 10L, config = sci_config_default())
```

## Arguments

- evidence:

  A validated evidence data.frame (from `r4subcore`).

- n_steps:

  Integer number of degradation steps after the baseline. Must be at
  least 1.

- config:

  An `sci_config` from
  [`sci_config_default()`](https://r4sub.github.io/r4subscore/reference/sci_config_default.md).

## Value

A tibble with one row per step and columns:

- `step`: integer step index, starting at 0

- `n_degraded`: number of passing checks turned into failures

- `frac_degraded`: `n_degraded` divided by the baseline passing count

- `SCI`: the Submission Confidence Index at that step

- `band`: the decision band at that step

- `delta`: change in SCI from the previous step (`NA` at step 0)

## Details

Starting from the supplied evidence, the function repeatedly converts a
growing number of currently-passing checks to `"fail"` and recomputes
the SCI at each step. Passing checks are degraded in order of how much
they contribute to the baseline SCI, largest contribution first, so the
curve shows the steepest descent the evidence allows and is
reproducible. Checks that already `warn` or `fail` are left untouched.

A passing check contributes `1 - severity_weight` to its indicator
score, so ordering by contribution degrades the lower-severity passing
checks earliest. This is a property of the scoring model, not of this
function.

Step 0 is the unmodified evidence. The final step degrades every passing
check. When the number of passing checks is smaller than `n_steps`, the
number of steps is reduced to the number of passing checks so that each
step degrades at least one additional check.

## See also

[`sci_monotone_check()`](https://r4sub.github.io/r4subscore/reference/sci_monotone_check.md)
to confirm a curve never rises.

## Examples

``` r
if (FALSE) { # \dontrun{
curve <- sci_degradation_curve(evidence, n_steps = 8)
curve
} # }
```
