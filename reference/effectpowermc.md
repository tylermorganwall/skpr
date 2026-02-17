# Fit Anova for Effect Power Calculation in Monte Carlo

Calculates the p-values for the effect power calculation in Monte Carlo

## Usage

``` r
effectpowermc(
  fit,
  type = "III",
  test = "Pr(>Chisq)",
  model_formula = NULL,
  firth = FALSE,
  glmfamily = "gaussian",
  effect_terms = NULL,
  RunMatrixReduced = NULL,
  method = NULL,
  contrastslist = contrastslist,
  effect_anova = FALSE,
  ...
)
```

## Arguments

- fit:

  Fit from regression

- type:

  Default `III`

- test:

  Default `Pr(>Chisq)`.

- ...:

  Additional arguments to pass to car::Anova

## Value

p-values
