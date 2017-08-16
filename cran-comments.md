## Test environments
* local Windows install, R 3.3.3
* local OS X install, R 3.2 
* ubuntu 14.04 (on travis-ci), R 3.4.1 
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

* checking DESCRIPTION meta-information ... NOTE
Versioned 'LinkingTo' value for 'RcppArmadillo' is only usable in R >= 3.0.2

This version of RcppArmadillo is required for this package to function.

