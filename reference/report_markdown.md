# Render an SCI Report as a Markdown Readiness Memo

Turns the object returned by
[`sci_report()`](https://r4sub.github.io/r4subscore/reference/sci_report.md)
into a single shareable Markdown string: a readiness memo that states
the Submission Confidence Index and its decision band, the distance to
target when one was set, the per-pillar breakdown, the findings most
worth acting on, and the pillars that offer the biggest improvement.
Where
[`print.sci_report()`](https://r4sub.github.io/r4subscore/reference/print.sci_report.md)
writes a styled console view, this produces a document you can paste
into a pull request, an issue, a status email, or a wiki.

## Usage

``` r
report_markdown(x, ...)

# S3 method for class 'sci_report'
report_markdown(x, title = "Submission Readiness Memo", ...)
```

## Arguments

- x:

  An `sci_report` object from
  [`sci_report()`](https://r4sub.github.io/r4subscore/reference/sci_report.md).

- ...:

  Passed to methods; currently unused.

- title:

  Heading for the memo. Default `"Submission Readiness Memo"`.

## Value

A length-one character string of Markdown, invisibly safe to
[`cat()`](https://rdrr.io/r/base/cat.html),
[`writeLines()`](https://rdrr.io/r/base/writeLines.html), or write to a
`.md` file.

## Details

The renderer reads the report object only; it does not recompute the
SCI, the band, or the gate, and it changes no scoring semantics. The
rendered memo carries an explicit line stating that it is decision
support, not a regulatory filing document.

`report_markdown()` is generic so other report-like objects can supply
their own method; the method for `sci_report` is provided here.

## Examples

``` r
ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY1", "DEV"))
ev <- suppressMessages(r4subcore::as_evidence(
  data.frame(
    asset_type = "validation", asset_id = "ADSL",
    source_name = "pinnacle21",
    indicator_id = c("Q1", "Q2", "T1", "R1", "U1"),
    indicator_name = c("Missing var", "Label", "Trace", "Risk", "Reviewer guide"),
    indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
    severity = c("high", "low", "medium", "high", "low"),
    result = c("fail", "pass", "warn", "fail", "pass"),
    stringsAsFactors = FALSE
  ),
  ctx = ctx
))
rpt <- sci_report(ev, targets = "ready")
md <- report_markdown(rpt)
cat(md)
#> # Submission Readiness Memo
#> 
#> _This is a decision-support summary, not a regulatory filing document._
#> 
#> Generated: 2026-11-15 07:27:47 UTC
#> 
#> ## Overall
#> 
#> - **Submission Confidence Index (SCI):** 30.6 / 100
#> - **Decision band:** high_risk
#> - **Target:** 54.4 SCI points below 85 (band ready).
#> 
#> ## Pillars
#> 
#> | Pillar | Score | Weight | Contribution |
#> | --- | ---: | ---: | ---: |
#> | quality | 37.5 | 0.35 | 13.1 |
#> | trace | 25 | 0.25 | 6.2 |
#> | risk | 0 | 0.25 | 0 |
#> | usability | 75 | 0.15 | 11.2 |
#> 
#> ## Findings to act on (3 findings)
#> 
#> | Severity | Result | Indicator | Domain | Detail |
#> | --- | --- | --- | --- | --- |
#> | high | fail | Q1 Missing var | quality |  |
#> | high | fail | R1 Risk | risk |  |
#> | medium | warn | T1 Trace | trace |  |
#> 
#> ## Biggest levers
#> 
#> - **risk:** up to 25 SCI points
#> - **quality:** up to 21.9 SCI points
#> - **trace:** up to 18.8 SCI points
#> 
```
