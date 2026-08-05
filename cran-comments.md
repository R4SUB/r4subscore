## Submission

This is an update of r4subscore from 0.1.0 (on CRAN) to 0.2.1, a feature release
for the R4SUB (Ready for Submission) ecosystem. Highlights:

* Per-indicator contribution breakdown and a what-if planner.
* A bootstrap confidence interval for the Submission Confidence Index.
* A non-compensatory gate that caps the band when a critical finding is present.
* Snapshot and diff functions for tracking readiness across runs.

See NEWS.md for the complete list.

## Test environments

* local: Windows 11 x64, R 4.5.x
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

r4subscore is imported by the r4sub meta-package and suggested by r4subpharma
and r4subui. Changes are additive and existing interfaces are unchanged.
