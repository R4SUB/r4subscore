# Count Severity-Weighted Conformance Findings

Summarises an evidence table into an independent count of unresolved
conformance findings, weighted by severity. This is the kind of figure a
reviewer reads off a validation report, and it is computed without the
SCI pillar weights, so it can serve as an external reference when
assessing the concurrent validity of the SCI.

## Usage

``` r
conformance_findings(evidence)
```

## Arguments

- evidence:

  A validated evidence data.frame (from `r4subcore`).

## Value

A single non-negative numeric value. Higher means more, or more severe,
unresolved findings.

## Details

Each non-passing check contributes its severity weight
([`r4subcore::severity_to_weight()`](https://r4sub.github.io/r4subcore/reference/severity_to_weight.html))
to the total. A `warn` contributes half as much as a `fail` of the same
severity. Passing checks contribute nothing.

## Examples

``` r
if (FALSE) { # \dontrun{
conformance_findings(evidence)
} # }
```
