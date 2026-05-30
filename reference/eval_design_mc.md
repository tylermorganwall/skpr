# Monte Carlo Power Evaluation for Experimental Designs

Evaluates the power of an experimental design, given the run matrix and
the statistical model to be fit to the data, using monte carlo
simulation. Simulated data is fit using a generalized linear model and
power is estimated by the fraction of times a parameter is significant.
Returns a data frame of parameter powers.

## Usage

``` r
eval_design_mc(
  design,
  model = NULL,
  alpha = 0.05,
  blocking = NULL,
  nsim = 1000,
  glmfamily = "gaussian",
  calceffect = TRUE,
  effect_anova = TRUE,
  varianceratios = NULL,
  rfunction = NULL,
  anticoef = NULL,
  firth = FALSE,
  effectsize = 2,
  contrasts = contr.sum,
  high_resolution_candidate_set = NA,
  moment_sample_density = 10,
  parallel = FALSE,
  adjust_alpha_inflation = FALSE,
  detailedoutput = FALSE,
  progress = TRUE,
  advancedoptions = NULL,
  ...
)
```

## Arguments

- design:

  The experimental design. Internally, `eval_design_mc` rescales each
  numeric column to the range -1 to 1.

- model:

  The model used in evaluating the design. If this is missing and the
  design was generated with skpr, the generating model will be used. It
  can be a subset of the model used to generate the design, or include
  higher order effects not in the original design generation. It cannot
  include factors that are not present in the experimental design.

- alpha:

  Default `0.05`. The type-I error. p-values less than this will be
  counted as significant.

- blocking:

  Default `NULL`. If `TRUE`, `eval_design_mc` will look at the rownames
  (or blocking columns) to determine blocking structure. Default FALSE.

- nsim:

  Default `1000`. The number of Monte Carlo simulations to perform.

- glmfamily:

  Default `gaussian`. String indicating the family of distribution for
  the `glm` function ("gaussian", "binomial", "poisson", or
  "exponential").

- calceffect:

  Default `TRUE`. Whether to calculate effect power. This calculation is
  more expensive than parameter power, so turned off (if not needed) can
  greatly speed up calculation time.

- effect_anova:

  Default `TRUE`, whether to a Type-III Anova or a likelihood ratio test
  to calculate effect power. If `TRUE`, effect power will be calculated
  using a Type-III Anova (using the car package) and a Wald test. If
  `FALSE`, a likelihood ratio test (using a reduced model for each
  effect) will performed using the `lmtest` package. If `firth = TRUE`,
  this will be set to `FALSE` automatically.

- varianceratios:

  Default `NULL`. The ratio of the whole plot variance to the run-to-run
  variance. If not specified during design generation, this will default
  to 1. For designs with more than one subplot this ratio can be a
  vector specifying the variance ratio for each subplot (comparing to
  the run-to-run variance). Otherwise, it will use a single value for
  all strata.

- rfunction:

  Default `NULL`.Random number generator function for the response
  variable. Should be a function of the form f(X, b, delta), where X is
  the model matrix, b are the anticipated coefficients, and delta is a
  vector of blocking errors. Typically something like
  `function(X, b, delta) rnorm(nrow(X), X %*% b + delta, 1)`. You only
  need to specify this if you do not like the default behavior described
  below.

- anticoef:

  Default `NULL`.The anticipated coefficients for calculating the power.
  If missing, coefficients will be automatically generated based on the
  `effectsize` argument.

- firth:

  Default `FALSE`. Whether to apply the firth correction (via the
  `mbest` package) to a logistic regression. This setting also
  automatically sets `effect_lr = TRUE`.

- effectsize:

  Helper argument to generate anticipated coefficients. See details for
  more info. If you specify `anticoef`, `effectsize` will be ignored.

- contrasts:

  Default `contr.sum`. The contrasts to use for categorical factors. If
  the user has specified their own contrasts for a categorical factor
  using the contrasts function, those will be used. Otherwise, skpr will
  use contr.sum.

- high_resolution_candidate_set:

  Default `NA`. If you have continuous numeric terms and disallowed
  combinations, the closed-form I-optimality value cannot be calculated
  and must be approximated by numeric integration. This requires
  sampling the allowed space densely, but most candidate sets will
  provide a sparse sampling of allowable points. To work around this,
  skpr will generate a convex hull of the numeric terms for each unique
  combination of categorical factors to generate a dense sampling of the
  space and cache that value internally, but this is a slow calculation
  and does not support non-convex candidate sets. To speed up moment
  matrix calculation, pass a higher resolution version of your candidate
  set here with the disallowed combinations already applied. If you
  generated your design externally from skpr, there are disallowed
  combinations in your design, and need correct I-optimality values, you
  must pass your candidate set here.

- moment_sample_density:

  Default `10`. The density of points to sample when calculating the
  moment matrix to compute I-optimality. Only required if the design was
  generated outside of skpr and there are disallowed combinations.

- parallel:

  Default `FALSE`. If `TRUE`, the Monte Carlo power calculation will use
  all but one of the available cores. If the user wants to set the
  number of cores manually, they can do this by setting
  `options("cores")` to the desired number (e.g.
  `options("cores" = parallel::detectCores())`). NOTE: If you have
  installed BLAS libraries that include multicore support (e.g. Intel
  MKL that comes with Microsoft R Open), turning on parallel could
  result in reduced performance.

- adjust_alpha_inflation:

  Default `FALSE`. If `TRUE`, this will run the simulation twice: first
  to calculate the empirical distribution of p-values under the null
  hypothesis and find the true Type-I error cutoff that corresponds to
  the desired Type-I error rate, and then again given effect size to
  calculate power values.

- detailedoutput:

  Default `FALSE`. If `TRUE`, return additional information about
  evaluation in results.

- progress:

  Default `TRUE`. Whether to include a progress bar.

- advancedoptions:

  Default `NULL`. Named list of advanced options.
  `advancedoptions$anovatype` specifies the Anova type in the car
  package (default type `III`), user can change to type `II`).
  `advancedoptions$anovatest` specifies the test statistic if the user
  does not want a `Wald` test–other options are likelihood-ratio `LR`
  and F-test `F`. `advancedoptions$progressBarUpdater` is a function
  called in non-parallel simulations that can be used to update external
  progress bar.`advancedoptions$GUI` turns off some warning messages
  when in the GUI. If `advancedoptions$save_simulated_responses = TRUE`,
  the dataframe will have an attribute `simulated_responses` that
  contains the simulated responses from the power evaluation.
  `advancedoptions$ci_error_conf` will set the confidence level for
  power intervals, which are printed when `detailedoutput = TRUE`.

- ...:

  Additional arguments.

## Value

A data frame consisting of the parameters and their powers, with
supplementary information stored in the data frame's attributes. The
parameter estimates from the simulations are stored in the "estimates"
attribute. The "model_matrix" attribute contains the model matrix that
was used for power evaluation, and also provides the encoding used for
categorical factors. If you want to specify the anticipated coefficients
manually, do so in the order the parameters appear in the model matrix.

## Details

Evaluates the power of a design with Monte Carlo simulation. Data is
simulated and then fit with a generalized linear model, and the fraction
of simulations in which a parameter is significant (its p-value,
according to the fit function used, is less than the specified `alpha`)
is the estimate of power for that parameter.

First, if `blocking = TURE`, the random noise from blocking is generated
with `rnorm`. Each block gets a single sample of Gaussian random noise,
with a variance as specified in `varianceratios`, and that sample is
copied to each run in the block. Then, `rfunction` is called to generate
a simulated response for each run of the design, and the data is fit
using the appropriate fitting function. The functions used to simulate
the data and fit it are determined by the `glmfamily` and `blocking`
arguments as follows. Below, X is the model matrix, b is the anticipated
coefficients, and d is a vector of blocking noise (if `blocking = FALSE`
then d = 0):

|  |  |  |  |
|----|----|----|----|
| **glmfamily** | **blocking** | **rfunction** | **fit** |
| "gaussian" | F | `rnorm(mean = X %*% b + d, sd = 1)` | `lm` |
| "gaussian" | T | `rnorm(mean = X %*% b + d, sd = 1)` | [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) |
| "binomial" | F | `rbinom(prob = 1/(1+exp(-(X %*% b + d))))` | `glm(family = "binomial")` |
| "binomial" | T | `rbinom(prob = 1/(1+exp(-(X %*% b + d))))` | `lme4::glmer(family = "binomial")` |
| "poisson" | F | `rpois(lambda = exp((X %*% b + d)))` | `glm(family = "poisson")` |
| "poisson" | T | `rpois(lambda = exp((X %*% b + d)))` | `lme4::glmer(family = "poisson")` |
| "exponential" | F | `rexp(rate = exp(-(X %*% b + d)))` | `glm(family = Gamma(link = "log"))` |
| "exponential" | T | `rexp(rate = exp(-(X %*% b + d)))` | `lme4:glmer(family = Gamma(link = "log"))` |

Note that the exponential random generator uses the "rate" parameter,
but `skpr` and `glm` use the mean value parameterization (= 1 / rate),
hence the minus sign above. Also note that the gaussian model assumes a
root-mean-square error of 1.

Power is dependent on the anticipated coefficients. You can specify
those directly with the `anticoef` argument, or you can use the
`effectsize` argument to specify an effect size and `skpr` will
auto-generate them. You can provide either a length-1 or length-2
vector. If you provide a length-1 vector, the anticipated coefficients
will be half of `effectsize`; this is equivalent to saying that the
*linear predictor* (for a gaussian model, the mean response; for a
binomial model, the log odds ratio; for an exponential model, the log of
the mean value; for a poisson model, the log of the expected response)
changes by `effectsize` when a continuous factor goes from its lowest
level to its highest level. If you provide a length-2 vector, the
anticipated coefficients will be set such that the *mean response* (for
a gaussian model, the mean response; for a binomial model, the
probability; for an exponential model, the mean response; for a poisson
model, the expected response) changes from `effectsize[1]` to
`effectsize[2]` when a factor goes from its lowest level to its highest
level, assuming that the other factors are inactive (their x-values are
zero).

The effect of a length-2 `effectsize` depends on the `glmfamily`
argument as follows:

For `glmfamily = 'gaussian'`, the coefficients are set to
`(effectsize[2] - effectsize[1]) / 2`.

For `glmfamily = 'binomial'`, the intercept will be
`1/2 * log(effectsize[1] * effectsize[2] / (1 - effectsize[1]) / (1 - effectsize[2]))`,
and the other coefficients will be
`1/2 * log(effectsize[2] * (1 - effectsize[1]) / (1 - effectsize[2]) / effectsize[1])`.

For `glmfamily = 'exponential'` or `'poisson'`, the intercept will be
`1 / 2 * (log(effectsize[2]) + log(effectsize[1]))`, and the other
coefficients will be
`1 / 2 * (log(effectsize[2]) - log(effectsize[1]))`.

## Examples

``` r
#We first generate a full factorial design using expand.grid:
factorialcoffee = expand.grid(cost = c(-1, 1),
                              type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                              size = as.factor(c("Short", "Grande", "Venti")))
if(skpr:::run_documentation()) {
#And then generate the 21-run D-optimal design using gen_design.
designcoffee = gen_design(factorialcoffee,
                         model = ~cost + type + size, trials = 21, optimality = "D")
}
if(skpr:::run_documentation()) {
#To evaluate this design using a normal approximation, we just use eval_design
#(here using the default settings for contrasts, effectsize, and the anticipated coefficients):

eval_design(design = designcoffee, model = ~cost + type + size, 0.05)
}
#>      parameter            type     power
#> 1  (Intercept)    effect.power 0.9871086
#> 2         cost    effect.power 0.9871086
#> 3         type    effect.power 0.9239395
#> 4         size    effect.power 0.8484877
#> 5  (Intercept) parameter.power 0.9871086
#> 6         cost parameter.power 0.9871086
#> 7        type1 parameter.power 0.6652414
#> 8        type2 parameter.power 0.6652414
#> 9        type3 parameter.power 0.6652414
#> 10       size1 parameter.power 0.8434093
#> 11       size2 parameter.power 0.8434093
#> ============Evaluation Info=============
#> • Alpha = 0.05 • Trials = 21 • Blocked = FALSE 
#> • Evaluating Model = ~cost + type + size 
#> • Anticipated Coefficients = c(1, 1, 1, -1, 1, 1, -1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
if(skpr:::run_documentation()) {
#To evaluate this design with a Monte Carlo method, we enter the same information
#used in eval_design, with the addition of the number of simulations "nsim" and the distribution
#family used in fitting for the glm "glmfamily". For gaussian, binomial, exponential, and poisson
#families, a default random generating function (rfunction) will be supplied. If another glm
#family is used or the default random generating function is not adequate, a custom generating
#function can be supplied by the user. Like in `eval_design()`, if the model isn't entered, the
#model used in generating the design will be used.

eval_design_mc(designcoffee, nsim = 100, glmfamily = "gaussian")
}
#>      parameter               type power
#> 1  (Intercept)    effect.power.mc  0.98
#> 2         cost    effect.power.mc  0.99
#> 3         type    effect.power.mc  0.92
#> 4         size    effect.power.mc  0.84
#> 5  (Intercept) parameter.power.mc  0.98
#> 6         cost parameter.power.mc  0.99
#> 7        type1 parameter.power.mc  0.62
#> 8        type2 parameter.power.mc  0.62
#> 9        type3 parameter.power.mc  0.69
#> 10       size1 parameter.power.mc  0.80
#> 11       size2 parameter.power.mc  0.82
#> ============Evaluation Info============
#> • Alpha = 0.05 • Trials = 21 • Blocked = FALSE 
#> • Evaluating Model = ~cost + type + size 
#> • Anticipated Coefficients = c(1, 1, 1, -1, 1, 1, -1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
if(skpr:::run_documentation()) {
#We can also add error bars on the Monte Carlo power values by setting
#`detailedoutput = TRUE` (which will print out other information as well).
#We can set the confidence via the `advancedoptions` argument.
eval_design_mc(designcoffee, nsim = 100, glmfamily = "gaussian",
              detailedoutput = TRUE, advancedoptions = list(ci_error_conf  = 0.8))
}
#>      parameter               type power anticoef alpha glmfamily trials nsim
#> 1  (Intercept)    effect.power.mc  1.00       NA  0.05  gaussian     21  100
#> 2         cost    effect.power.mc  0.98       NA  0.05  gaussian     21  100
#> 3         type    effect.power.mc  0.97       NA  0.05  gaussian     21  100
#> 4         size    effect.power.mc  0.87       NA  0.05  gaussian     21  100
#> 5  (Intercept) parameter.power.mc  1.00        1  0.05  gaussian     21  100
#> 6         cost parameter.power.mc  0.98        1  0.05  gaussian     21  100
#> 7        type1 parameter.power.mc  0.61        1  0.05  gaussian     21  100
#> 8        type2 parameter.power.mc  0.74       -1  0.05  gaussian     21  100
#> 9        type3 parameter.power.mc  0.72        1  0.05  gaussian     21  100
#> 10       size1 parameter.power.mc  0.86        1  0.05  gaussian     21  100
#> 11       size2 parameter.power.mc  0.84       -1  0.05  gaussian     21  100
#>    blocking error_adjusted_alpha power_lcb power_ucb
#> 1     FALSE                 0.05 0.9772372 1.0000000
#> 2     FALSE                 0.05 0.9476547 0.9946693
#> 3     FALSE                 0.05 0.9344142 0.9889293
#> 4     FALSE                 0.05 0.8161255 0.9120174
#> 5     FALSE                 0.05 0.9772372 1.0000000
#> 6     FALSE                 0.05 0.9476547 0.9946693
#> 7     FALSE                 0.05 0.5412640 0.6753363
#> 8     FALSE                 0.05 0.6754202 0.7970937
#> 9     FALSE                 0.05 0.6544316 0.7787205
#> 10    FALSE                 0.05 0.8049623 0.9035414
#> 11    FALSE                 0.05 0.7828449 0.8863619
#> =======================================================Evaluation Info========================================================
#> • Alpha = 0.05 • Trials = 21 • Blocked = FALSE 
#> • Evaluating Model = ~cost + type + size 
#> • Anticipated Coefficients = c(1, 1, 1, -1, 1, 1, -1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
#> • MC Power CI Confidence = 80% 
if(skpr:::run_documentation()) {
#We see here we generate approximately the same parameter powers as we do
#using the normal approximation in eval_design. Like eval_design, we can also change
#effectsize to produce a different signal-to-noise ratio:

eval_design_mc(design = designcoffee, nsim = 100,
                       glmfamily = "gaussian", effectsize = 1)
}
#>      parameter               type power
#> 1  (Intercept)    effect.power.mc  0.56
#> 2         cost    effect.power.mc  0.56
#> 3         type    effect.power.mc  0.33
#> 4         size    effect.power.mc  0.32
#> 5  (Intercept) parameter.power.mc  0.56
#> 6         cost parameter.power.mc  0.56
#> 7        type1 parameter.power.mc  0.23
#> 8        type2 parameter.power.mc  0.24
#> 9        type3 parameter.power.mc  0.18
#> 10       size1 parameter.power.mc  0.36
#> 11       size2 parameter.power.mc  0.32
#> ============Evaluation Info============
#> • Alpha = 0.05 • Trials = 21 • Blocked = FALSE 
#> • Evaluating Model = ~cost + type + size 
#> • Anticipated Coefficients = c(0.500, 0.500, 0.500, -0.500, 0.500, 0.500, -0.500) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
if(skpr:::run_documentation()) {
#Like eval_design, we can also evaluate the design with a different model than
#the one that generated the design.
eval_design_mc(design = designcoffee, model = ~cost + type, alpha = 0.05,
              nsim = 100, glmfamily = "gaussian")
}
#>     parameter               type power
#> 1 (Intercept)    effect.power.mc  0.99
#> 2        cost    effect.power.mc  0.97
#> 3        type    effect.power.mc  0.88
#> 4 (Intercept) parameter.power.mc  0.99
#> 5        cost parameter.power.mc  0.97
#> 6       type1 parameter.power.mc  0.74
#> 7       type2 parameter.power.mc  0.66
#> 8       type3 parameter.power.mc  0.57
#> ===========Evaluation Info============
#> • Alpha = 0.05 • Trials = 21 • Blocked = FALSE 
#> • Evaluating Model = ~cost + type 
#> • Anticipated Coefficients = c(1, 1, 1, -1, 1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
if(skpr:::run_documentation()) {
#And here it is evaluated with additional interactions included:
eval_design_mc(design = designcoffee, model = ~cost + type + size + cost * type, 0.05,
              nsim = 100, glmfamily = "gaussian")
}
#>      parameter               type power
#> 1  (Intercept)    effect.power.mc  0.98
#> 2         cost    effect.power.mc  0.98
#> 3         type    effect.power.mc  0.93
#> 4         size    effect.power.mc  0.88
#> 5    cost:type    effect.power.mc  0.83
#> 6  (Intercept) parameter.power.mc  0.98
#> 7         cost parameter.power.mc  0.98
#> 8        type1 parameter.power.mc  0.70
#> 9        type2 parameter.power.mc  0.67
#> 10       type3 parameter.power.mc  0.64
#> 11       size1 parameter.power.mc  0.82
#> 12       size2 parameter.power.mc  0.84
#> 13  cost:type1 parameter.power.mc  0.63
#> 14  cost:type2 parameter.power.mc  0.63
#> 15  cost:type3 parameter.power.mc  0.58
#> ============Evaluation Info============
#> • Alpha = 0.05 • Trials = 21 • Blocked = FALSE 
#> • Evaluating Model = ~cost + type + size + cost * type 
#> • Anticipated Coefficients = c(1, 1, 1, -1, 1, 1, -1, 1, -1, 1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
if(skpr:::run_documentation()) {
#We can also set "parallel = TRUE" to use all the cores available to speed up
#computation.
eval_design_mc(design = designcoffee, nsim = 10000,
                       glmfamily = "gaussian", parallel = TRUE)
}
#> Using user-defined {future} plan() `plan(sequential)` instead of {skpr}'s default multicore plan (for this computer) of `plan("multicore", workers = number_of_cores-1)`
#> Error in globalCallingHandlers(condition = global_progression_handler): should not be called with handlers on the stack
if(skpr:::run_documentation()) {
#We can also evaluate split-plot designs. First, let us generate the split-plot design:

factorialcoffee2 = expand.grid(Temp = c(1, -1),
                               Store = as.factor(c("A", "B")),
                               cost = c(-1, 1),
                               type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                               size = as.factor(c("Short", "Grande", "Venti")))

vhtcdesign = gen_design(factorialcoffee2,
                       model = ~Store, trials = 6, varianceratio = 1)
htcdesign = gen_design(factorialcoffee2, model = ~Store + Temp, trials = 18,
                       splitplotdesign = vhtcdesign, blocksizes = rep(3, 6), varianceratio = 1)
splitplotdesign = gen_design(factorialcoffee2,
                            model = ~Store + Temp + cost + type + size, trials = 54,
                            splitplotdesign = htcdesign, blocksizes = rep(3, 18),
                            varianceratio = 1)

#Each block has an additional noise term associated with it in addition to the normal error
#term in the model. This is specified by a vector specifying the additional variance for
#each split-plot level. This is equivalent to specifying a variance ratio of one between
#the whole plots and the run-to-run variance for gaussian models.

#Evaluate the design. Note the decreased power for the blocking factors.
eval_design_mc(splitplotdesign, blocking = TRUE, nsim = 100,
                       glmfamily = "gaussian", varianceratios = c(1, 1, 1))
}
#>      parameter               type power
#> 1        Store    effect.power.mc  0.38
#> 2         Temp    effect.power.mc  0.82
#> 3         cost    effect.power.mc  1.00
#> 4         type    effect.power.mc  1.00
#> 5         size    effect.power.mc  1.00
#> 6  (Intercept) parameter.power.mc  0.34
#> 7       Store1 parameter.power.mc  0.38
#> 8         Temp parameter.power.mc  0.82
#> 9         cost parameter.power.mc  1.00
#> 10       type1 parameter.power.mc  0.99
#> 11       type2 parameter.power.mc  0.96
#> 12       type3 parameter.power.mc  0.98
#> 13       size1 parameter.power.mc  1.00
#> 14       size2 parameter.power.mc  1.00
#> ============Evaluation Info============
#> • Alpha = 0.05 • Trials = 54 • Blocked = TRUE 
#> • Evaluating Model = ~Store + Temp + cost + type + size 
#> • Anticipated Coefficients = c(1, 1, 1, 1, 1, -1, 1, 1, -1) 
#> • Number of Blocks = Level :  
#> • Variance Ratios  = Level 1: 1, Level 2: 1, Level 3: 1 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lmerTest::lmer(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
if(skpr:::run_documentation()) {
#We can also use this method to evaluate designs that cannot be easily
#evaluated using normal approximations. Here, we evaluate a design with a binomial response and see
#whether we can detect the difference between each factor changing whether an event occurs
#70% of the time or 90% of the time.

factorialbinom = expand.grid(a = c(-1, 1), b = c(-1, 1))
designbinom = gen_design(factorialbinom, model = ~a + b, trials = 90, optimality = "D")

eval_design_mc(designbinom, ~a + b, alpha = 0.2, nsim = 100, effectsize = c(0.7, 0.9),
              glmfamily = "binomial")
}
#>     parameter               type power
#> 1 (Intercept)    effect.power.mc  1.00
#> 2           a    effect.power.mc  0.90
#> 3           b    effect.power.mc  0.89
#> 4 (Intercept) parameter.power.mc  1.00
#> 5           a parameter.power.mc  0.90
#> 6           b parameter.power.mc  0.89
#> ===========Evaluation Info============
#> • Alpha = 0.2 • Trials = 90 • Blocked = FALSE 
#> • Evaluating Model = ~a + b 
#> • Anticipated Coefficients = c(1.522, 0.675, 0.675) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = glm(..., family = "binomial")` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
if(skpr:::run_documentation()) {
#We can also use this method to determine power for poisson response variables.
#Generate the design:

factorialpois = expand.grid(a = as.numeric(c(-1, 0, 1)), b = c(-1, 0, 1))
designpois = gen_design(factorialpois, ~a + b, trials = 70, optimality = "D")

#Evaluate the power:

eval_design_mc(designpois, ~a + b, 0.05, nsim = 100, glmfamily = "poisson",
               anticoef = log(c(0.2, 2, 2)))
}
#>     parameter               type power
#> 1 (Intercept)    effect.power.mc  0.95
#> 2           a    effect.power.mc  0.80
#> 3           b    effect.power.mc  0.82
#> 4 (Intercept) parameter.power.mc  0.95
#> 5           a parameter.power.mc  0.80
#> 6           b parameter.power.mc  0.82
#> ===========Evaluation Info============
#> • Alpha = 0.05 • Trials = 70 • Blocked = FALSE 
#> • Evaluating Model = ~a + b 
#> • Anticipated Coefficients = c(-1.609, 0.693, 0.693) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `glm(..., family = "poisson")` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#The coefficients above set the nominal value -- that is, the expected count
#when all inputs = 0 -- to 0.2 (from the intercept), and say that each factor
#changes this count by a factor of 4 (multiplied by 2 when x= +1, and divided by 2 when x = -1).
#Note the use of log() in the anticipated coefficients.
```
