# Extract p-values from a model object

Extract p-values from a model object. Currently works with lm, glm,
lme4, glmer, and survreg model objects. If possible, uses the p-values
reported in summary(model_fit).

## Usage

``` r
extractPvalues(model_fit, glmfamily = "gaussian")
```

## Arguments

- model_fit:

  The model object from which to extract.

## Value

Returns a vector of p-values. If model_fit is not a supported model
type, returns NULL.
