## Test environments
* local OS X install, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)

* This is a new release.

## R CMD check results

0 errors | 0 warnings | 1 note

There are no ERRORs or WARNINGs

There was 1 NOTE:

* checking R code for possible problems ... NOTE
plotCristals: no visible global function definition for ‘polygon’
plotHexagons: no visible global function definition for ‘polygon’
snowflakeWithHex: no visible global function definition for ‘rbeta’
snowflakes: no visible global function definition for ‘runif’
snowflakes: no visible global function definition for ‘par’
snowflakes: no visible global function definition for ‘rbeta’
transpCol: no visible global function definition for ‘colors’
transpCol: no visible global function definition for ‘col2rgb’
transpCol: no visible global function definition for ‘rgb’
Undefined global functions or variables:
  col2rgb colors par polygon rbeta rgb runif
Consider adding
  importFrom("grDevices", "col2rgb", "colors", "rgb")
  importFrom("graphics", "par", "polygon")
  importFrom("stats", "rbeta", "runif")
to your NAMESPACE file.

## Downstream dependencies

There are currently no downstream dependencies for this package

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---

