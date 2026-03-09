# Evaluate Power for Survival Design

Evaluates power for an experimental design in which the response
variable may be right- or left-censored. Power is evaluated with a Monte
Carlo simulation, using the `survival` package and `survreg` to fit the
data. Split-plot designs are not supported.

## Usage

``` r
eval_design_survival_mc(
  design,
  model = NULL,
  alpha = 0.05,
  nsim = 1000,
  distribution = "gaussian",
  censorpoint = NA,
  censortype = "right",
  rfunctionsurv = NULL,
  anticoef = NULL,
  effectsize = 2,
  contrasts = contr.sum,
  parallel = FALSE,
  detailedoutput = FALSE,
  progress = TRUE,
  advancedoptions = NULL,
  ...
)
```

## Arguments

- design:

  The experimental design. Internally, all numeric columns will be
  rescaled to -1 to 1.

- model:

  The model used in evaluating the design. If this is missing and the
  design was generated with skpr, the generating model will be used. It
  can be a subset of the model used to generate the design, or include
  higher order effects not in the original design generation. It cannot
  include factors that are not present in the experimental design.

- alpha:

  Default `0.05`. The type-I error. p-values less than this will be
  counted as significant.

- nsim:

  The number of simulations. Default 1000.

- distribution:

  Distribution of survival function to use when fitting the data. Valid
  choices are described in the documentation for `survreg`. *Supported*
  options are "exponential", "lognormal", or "gaussian". Default
  "gaussian".

- censorpoint:

  The point after/before (for right-censored or left-censored data,
  respectively) which data should be labelled as censored. Default NA
  for no censoring. This argument is used only by the internal random
  number generators; if you supply your own function to the
  `rfunctionsurv` parameter, then this parameter will be ignored.

- censortype:

  The type of censoring (either "left" or "right"). Default "right".

- rfunctionsurv:

  Random number generator function. Should be a function of the form
  f(X, b), where X is the model matrix and b are the anticipated
  coefficients. This function should return a `Surv` object from the
  `survival` package. You do not need to provide this argument if
  `distribution` is one of the supported choices and you are satisfied
  with the default behavior described below.

- anticoef:

  The anticipated coefficients for calculating the power. If missing,
  coefficients will be automatically generated based on the `effectsize`
  argument.

- effectsize:

  Helper argument to generate anticipated coefficients. See details for
  more info. If you specify `anticoef`, `effectsize` will be ignored.

- contrasts:

  Default `contr.sum`. Function used to encode categorical variables in
  the model matrix. If the user has specified their own contrasts for a
  categorical factor using the contrasts function, those will be used.
  Otherwise, skpr will use contr.sum.

- parallel:

  Default `FALSE`. If `TRUE`, the power simulation will use all but one
  of the available cores. If the user wants to set the number of cores
  manually, they can do this by setting `options("cores")` to the
  desired number (e.g. `options("cores" = parallel::detectCores())`).
  NOTE: If you have installed BLAS libraries that include multicore
  support (e.g. Intel MKL that comes with Microsoft R Open), turning on
  parallel could result in reduced performance.

- detailedoutput:

  Default `FALSE`. If `TRUE`, return additional information about
  evaluation in results.

- progress:

  Default `TRUE`. Whether to include a progress bar.

- advancedoptions:

  Default `NULL`. Named list of advanced options. Pass
  `progressBarUpdater` to include function called in non-parallel
  simulations that can be used to update external progress bar.
  `advancedoptions$ci_error_conf` will set the confidence level for
  power intervals, which are printed when `detailedoutput = TRUE`.

- ...:

  Any additional arguments to be passed into the `survreg` function
  during fitting.

## Value

A data frame consisting of the parameters and their powers. The
parameter estimates from the simulations are stored in the 'estimates'
attribute. The 'modelmatrix' attribute contains the model matrix and the
encoding used for categorical factors. If you manually specify
anticipated coefficients, do so in the order of the model matrix.

## Details

Evaluates the power of a design with Monte Carlo simulation. Data is
simulated and then fit with a survival model
([`survival::survreg`](https://rdrr.io/pkg/survival/man/survreg.html)),
and the fraction of simulations in which a parameter is significant (its
p-value is less than the specified `alpha`) is the estimate of power for
that parameter.

If not supplied by the user, `rfunctionsurv` will be generated based on
the `distribution` argument as follows:

|                  |                                        |
|------------------|----------------------------------------|
| **distribution** | **generating function**                |
| "gaussian"       | `rnorm(mean = X %*% b, sd = 1)`        |
| "exponential"    | `rexp(rate = exp(-X %*% b))`           |
| "lognormal"      | `rlnorm(meanlog = X %*% b, sdlog = 1)` |

In each case, if a simulated data point is past the censorpoint (greater
than for right-censored, less than for left-censored) it is marked as
censored. See the examples below for how to construct your own function.

Power is dependent on the anticipated coefficients. You can specify
those directly with the `anticoef` argument, or you can use the
`effectsize` argument to specify an effect size and `skpr` will
auto-generate them. You can provide either a length-1 or length-2
vector. If you provide a length-1 vector, the anticipated coefficients
will be half of `effectsize`; this is equivalent to saying that the
*linear predictor* (for a gaussian model, the mean response; for an
exponential model or lognormal model, the log of the mean value) changes
by `effectsize` when a continuous factor goes from its lowest level to
its highest level. If you provide a length-2 vector, the anticipated
coefficients will be set such that the *mean response* changes from
`effectsize[1]` to `effectsize[2]` when a factor goes from its lowest
level to its highest level, assuming that the other factors are inactive
(their x-values are zero).

The effect of a length-2 `effectsize` depends on the `distribution`
argument as follows:

For `distribution = 'gaussian'`, the coefficients are set to
`(effectsize[2] - effectsize[1]) / 2`.

For `distribution = 'exponential'` or `'lognormal'`, the intercept will
be `1 / 2 * (log(effectsize[2]) + log(effectsize[1]))`, and the other
coefficients will be
`1 / 2 * (log(effectsize[2]) - log(effectsize[1]))`.

## Examples

``` r
#These examples focus on the survival analysis case and assume familiarity
#with the basic functionality of eval_design_mc.

#We first generate a simple 2-level design using expand.grid:
basicdesign = expand.grid(a = c(-1, 1))
design = gen_design(candidateset = basicdesign, model = ~a, trials = 15)

#We can then evaluate the power of the design in the same way as eval_design_mc,
#now including the type of censoring (either right or left) and the point at which
#the data should be censored:

eval_design_survival_mc(design = design, model = ~a, alpha = 0.05,
                        nsim = 100, distribution = "exponential",
                        censorpoint = 5, censortype = "right")
#> Warning: default or length 1 delta used with distribution == 'exponential'. This can lead to unrealistic effect sizes - make sure the generated anticipated coeffcients are appropriate.
#>     parameter               type power
#> 1 (Intercept) parameter.power.mc  0.91
#> 2           a parameter.power.mc  0.89
#> ===========Evaluation Info============
#> • Alpha = 0.05 • Trials = 15 • Blocked = FALSE 
#> • Evaluating Model =  
#> • Anticipated Coefficients = c(1, 1) 

#Built-in Monte Carlo random generating functions are included for the gaussian, exponential,
#and lognormal distributions.

#We can also evaluate different censored distributions by specifying a custom
#random generating function and changing the distribution argument.

rlognorm = function(X, b) {
  Y = rlnorm(n = nrow(X), meanlog = X %*% b, sdlog = 0.4)
  censored = Y > 1.2
  Y[censored] = 1.2
  return(survival::Surv(time = Y, event = !censored, type = "right"))
}

#Any additional arguments are passed into the survreg function call.  As an example, you
#might want to fix the "scale" argument to survreg, when fitting a lognormal:

eval_design_survival_mc(design = design, model = ~a, alpha = 0.2, nsim = 100,
                        distribution = "lognormal", rfunctionsurv = rlognorm,
                        anticoef = c(0.184, 0.101), scale = 0.4)
#>     parameter               type power
#> 1 (Intercept) parameter.power.mc  0.51
#> 2           a parameter.power.mc  0.40
#> ===========Evaluation Info============
#> • Alpha = 0.2 • Trials = 15 • Blocked = FALSE 
#> • Evaluating Model =  
#> • Anticipated Coefficients = c(0.184, 0.101) 
```
