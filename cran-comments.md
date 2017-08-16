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

Additionally, there was another NOTE only on the Ubuntu system related to the package size:

* checking installed package size ... NOTE
installed size is  7.5Mb
sub-directories of 1Mb or more:
  libs   6.8Mb

This is due to RcppEigen/RcppArmadillo, which are required for this package to function.