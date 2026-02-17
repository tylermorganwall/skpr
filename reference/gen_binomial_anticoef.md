# Generates Binomial Anticipated Coefficients

Generates Binomial Anticipated Coefficients Solves the logistic function
log(p / (1-p)) = beta0 + beta1 \* x such that p = lowprob when x = -1,
and p = highprob when x = +1. Equivalently, solves this set of equations
for beta0 and beta1: log(lowprob / (1 - lowprob)) = beta0 - beta1
log(highprob / (1 - highprob)) = beta0 + beta1

## Usage

``` r
gen_binomial_anticoef(anticoef, lowprob, highprob)
```

## Arguments

- anticoef:

  Anticipated coefficients

- lowprob:

  Default 0.50. The base probability

- highprob:

  Default 0.80. The high probability

## Value

Anticipated coefficients.
