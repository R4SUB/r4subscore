# Case study: scoring a CDISC pilot for submission readiness

You have run Pinnacle 21, checked your Define-XML, and looked over the
datasets. The output is a few hundred findings spread across a dozen
tools. The question the study team actually asks is simpler than any
single report answers: are we ready to file?

This is what the Submission Confidence Index (SCI) is for. It takes a
table of evidence and returns one number between 0 and 100, plus a
decision band, plus a breakdown of where the number comes from. Here we
walk a realistic CDISC pilot study through it, using the example
evidence that ships with `r4subdata`.

``` r

library(r4subscore)
library(r4subdata)
```

## The evidence

`evidence_pharma` is a validation run for a CDISC pilot study: 250
findings from tools like Pinnacle 21, spread across the four readiness
pillars.

``` r

ev <- evidence_pharma
nrow(ev)
#> [1] 250
table(ev$indicator_domain, ev$result)
#>            
#>             fail na pass warn
#>   quality     26 14   55   25
#>   risk         6  2   24    7
#>   trace        9  1   29    8
#>   usability    8  3   26    7
```

Each row is one check against one asset, carrying a severity and a
result. That is the whole contract. Any tool that can write this shape
feeds the score.

## The score

Three calls take the evidence to a number: indicators, pillars, then the
composite index.

``` r

ind <- compute_indicator_scores(ev)
ps  <- compute_pillar_scores(ev)
sci <- compute_sci(ps)

sci
#> ℹ Submission Confidence Index: 44.4
#> ℹ Decision Band: "high_risk"
#> ℹ   quality: 39.6 (weight: 0.35)
#> ℹ   trace: 43.9 (weight: 0.25)
#> ℹ   risk: 49.9 (weight: 0.25)
#> ℹ   usability: 47.2 (weight: 0.15)
```

The pillar scores show where the study stands on each front before they
are weighted together.

``` r

ps
#> # A tibble: 4 × 4
#>   pillar    pillar_score n_indicators weight
#>   <chr>            <dbl>        <int>  <dbl>
#> 1 quality          0.396            8   0.35
#> 2 trace            0.439            4   0.25
#> 3 risk             0.499            3   0.25
#> 4 usability        0.472            3   0.15
```

## Where the score comes from

A single number is only useful if you can open it up.
[`sci_explain()`](https://r4sub.github.io/r4subscore/reference/sci_explain.md)
reports each pillar’s contribution and, more usefully, ranks the
indicators dragging the score down.

``` r

expl <- sci_explain(ev)
expl$pillar_contributions
#> # A tibble: 4 × 6
#>   pillar    pillar_score n_indicators weight contribution  loss
#>   <chr>            <dbl>        <int>  <dbl>        <dbl> <dbl>
#> 1 quality          0.396            8   0.35         13.9  21.1
#> 2 trace            0.439            4   0.25         11    14  
#> 3 risk             0.499            3   0.25         12.5  12.5
#> 4 usability        0.472            3   0.15          7.1   7.9
head(expl$indicator_contributions, 8)
#> # A tibble: 8 × 6
#>   indicator_id     indicator_name     indicator_domain indicator_score  loss
#>   <chr>            <chr>              <chr>                      <dbl> <dbl>
#> 1 R-HIGH-RPN       High RPN Item      risk                       0.323  5.64
#> 2 T-ORPHAN-VAR     Orphan Variable    trace                      0.292  4.43
#> 3 T-TRACE-LEVEL    Traceability Level trace                      0.362  3.98
#> 4 R-MITIGATION-GAP Mitigation Gap     risk                       0.527  3.94
#> 5 Q-TYPE-MISMATCH  Type Mismatch      quality                    0.259  3.24
#> 6 Q-FORMAT-ERR     Format Error       quality                    0.321  2.97
#> 7 R-OPEN-RISK      Open Risk          risk                       0.648  2.94
#> 8 T-AMBIGUOUS-MAP  Ambiguous Mapping  trace                      0.533  2.92
#> # ℹ 1 more variable: pct_of_total_loss <dbl>
```

The indicators at the top of that list are where remediation buys the
most. A team can work the list from the top and watch the score move,
rather than guessing which of 250 findings matter.

## Is the score stable?

The default weights (quality 0.35, trace 0.25, risk 0.25, usability
0.15) are a choice, not a law. Before anyone leans on the number, it is
worth asking how much it depends on that choice.
[`sci_sensitivity_analysis()`](https://r4sub.github.io/r4subscore/reference/sci_sensitivity_analysis.md)
recomputes the SCI across a grid of alternative weightings.

``` r

grid <- data.frame(
  quality   = c(0.35, 0.50, 0.25, 0.40),
  trace     = c(0.25, 0.20, 0.30, 0.20),
  risk      = c(0.25, 0.20, 0.25, 0.30),
  usability = c(0.15, 0.10, 0.20, 0.10)
)

sci_sensitivity_analysis(ev, weight_grid = grid)
#> # A tibble: 4 × 7
#>   scenario quality trace  risk usability   SCI band     
#>      <int>   <dbl> <dbl> <dbl>     <dbl> <dbl> <chr>    
#> 1        1    0.35  0.25  0.25      0.15  44.4 high_risk
#> 2        2    0.5   0.2   0.2       0.1   43.3 high_risk
#> 3        3    0.25  0.3   0.25      0.2   45   high_risk
#> 4        4    0.4   0.2   0.3       0.1   44.3 high_risk
```

If the band holds across that grid, the readiness call is robust to the
weighting. If it flips between bands, the honest reading is that the
decision hangs on a judgment about which pillar matters most, and that
judgment should be made explicitly rather than buried in a default.

## What this answers

The SCI does not replace the underlying checks. It sits on top of them
and answers the one question the checks cannot: taken together, how
ready is this submission, and what would move the needle first. For a
study team deciding whether to file, that is the number that matters.
