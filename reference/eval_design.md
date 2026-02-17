# Calculate Power of an Experimental Design

Evaluates the power of an experimental design, for normal response
variables, given the design's run matrix and the statistical model to be
fit to the data. Returns a data frame of parameter and effect powers.
Designs can consist of both continuous and categorical factors. By
default, `eval_design` assumes a signal-to-noise ratio of 2, but this
can be changed with the `effectsize` or `anticoef` parameters.

## Usage

``` r
eval_design(
  design,
  model = NULL,
  alpha = 0.05,
  blocking = NULL,
  anticoef = NULL,
  effectsize = 2,
  varianceratios = NULL,
  contrasts = contr.sum,
  conservative = FALSE,
  reorder_factors = FALSE,
  detailedoutput = FALSE,
  advancedoptions = NULL,
  high_resolution_candidate_set = NA,
  moment_sample_density = 10,
  ...
)
```

## Arguments

- design:

  The experimental design. Internally, `eval_design` rescales each
  numeric column to the range -1 to 1.

- model:

  The model used in evaluating the design. If this is missing and the
  design was generated with skpr, the generating model will be used. It
  can be a subset of the model used to generate the design, or include
  higher order effects not in the original design generation. It cannot
  include factors that are not present in the experimental design.

- alpha:

  Default `0.05`. The specified type-I error.

- blocking:

  Default `NULL`. If `TRUE`, `eval_design` will look at the rownames (or
  blocking columns) to determine blocking structure. Default FALSE.

- anticoef:

  The anticipated coefficients for calculating the power. If missing,
  coefficients will be automatically generated based on the `effectsize`
  argument.

- effectsize:

  Default `2`. The signal-to-noise ratio. For continuous factors, this
  specifies the difference in response between the highest and lowest
  levels of the factor (which are -1 and +1 after `eval_design`
  normalizes the input data), assuming that the root mean square error
  is 1. If you do not specify `anticoef`, the anticipated coefficients
  will be half of `effectsize`. If you do specify `anticoef`,
  `effectsize` will be ignored.

- varianceratios:

  Default `NULL`. The ratio of the whole plot variance to the run-to-run
  variance. If not specified during design generation, this will default
  to 1. For designs with more than one subplot this ratio can be a
  vector specifying the variance ratio for each subplot (comparing to
  the run-to-run variance). Otherwise, it will use a single value for
  all strata.

- contrasts:

  Default `contr.sum`. The function to use to encode the categorical
  factors in the model matrix. If the user has specified their own
  contrasts for a categorical factor using the contrasts function, those
  will be used. Otherwise, skpr will use `contr.sum`.

- conservative:

  Specifies whether default method for generating anticipated
  coefficents should be conservative or not. `TRUE` will give the most
  conservative estimate of power by setting all but one (or multiple if
  they are equally low) level in each categorical factor's anticipated
  coefficients to zero. Default `FALSE`.

- reorder_factors:

  Default `FALSE`. If `TRUE`, the levels will be reordered to generate
  the most conservative calculation of effect power. The function
  searches through all possible reference levels for a given factor and
  chooses the one that results in the lowest effect power. The
  reordering will be presenting in the output when
  `detailedoutput = TRUE`.

- detailedoutput:

  If \`TRUE“, return additional information about evaluation in results.
  Default FALSE.

- advancedoptions:

  Default `NULL`. A named list with parameters to specify additional
  attributes to calculate. Options: `aliaspower` gives the degree at
  which the Alias matrix should be calculated.

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
  combinations in your design, and need correct I-optimalituy values,
  you must pass your candidate set here.

- moment_sample_density:

  Default `10`. The density of points to sample when calculating the
  moment matrix to compute I-optimality. Only required if the design was
  generated outside of skpr and there are disallowed combinations. It is
  much faster and potentially more accurate to provide your own
  candidate set with higher resolution continuous factors to
  `high_resolution_candidate_set`.\`

- ...:

  Additional arguments.

## Value

A data frame with the parameters of the model, the type of power
analysis, and the power. Several design diagnostics are stored as
attributes of the data frame. In particular, the `modelmatrix` attribute
contains the model matrix that was used for power evaluation. This is
especially useful if you want to specify the anticipated coefficients to
use for power evaluation. The model matrix provides the order of the
model coefficients, as well as the encoding used for categorical
factors.

## Details

This function evaluates the power of experimental designs.

If the design is has no blocking or restrictions on randomization, the
model assumed is:

\\y = X \beta + \epsilon\\.

If the design is a split-plot design, the model is as follows:

\\y = X \beta + Z b\\_(i) + \\\epsilon\\_(ij),

Here, \\y\\ is the vector of experimental responses, \\X\\ is the model
matrix, \\\beta\\ is the vector of model coefficients, \\Z\_{i}\\ are
the blocking indicator, \\b\_{i}\\ is the random variable associated
with the \\i\\th block, and \\\epsilon\\ is a random variable normally
distributed with zero mean and unit variance (root-mean-square error is
1.0).

`eval_design` calculates both parameter power as well as effect power,
defined as follows:

1.  Parameter power is the probability of rejecting the hypothesis \\H_0
    : \beta_i = 0\\, where \\\beta_i\\ is a single parameter in the
    model

2.  Effect power is the probability of rejecting the hypothesis \\H_0 :
    \beta\_{1} = \beta\_{2} = ... = \beta\_{n} 0\\ for all \\n\\
    coefficients for a categorical factor.

The two power types are equivalent for continuous factors and two-level
categorical factors, but they will differ for categorical factors with
three or more levels.

For split-plot designs, the degrees of freedom are allocated to each
term according to the algorithm given in "Mixed-Effects Models in S and
S-PLUS" (Pinheiro and Bates, pp. 91).

When using `conservative = TRUE`, `eval_design` first evaluates the
power with the default (or given) coefficients. Then, for each
multi-level categorical, it sets all coefficients to zero except the
level that produced the lowest power, and then re-evaluates the power
with this modified set of anticipated coefficients. If there are two or
more equal power values in a multi-level categorical, two of the lowest
equal terms are given opposite sign anticipated coefficients and the
rest (for that categorical factor) are set to zero.

## Examples

``` r
#Generating a simple 2x3 factorial to feed into our optimal design generation
#of an 11-run design.
factorial = expand.grid(A = c(1, -1), B = c(1, -1), C = c(1, -1))

optdesign = gen_design(candidateset = factorial,
                      model= ~A + B + C, trials = 11, optimality = "D", repeats = 100)

#Now evaluating that design (with default anticipated coefficients and a effectsize of 2):
eval_design(design = optdesign, model= ~A + B + C, alpha = 0.2)
#>     parameter            type     power
#> 1 (Intercept)    effect.power 0.9622638
#> 2           A    effect.power 0.9622638
#> 3           B    effect.power 0.9622638
#> 4           C    effect.power 0.9622638
#> 5 (Intercept) parameter.power 0.9622638
#> 6           A parameter.power 0.9622638
#> 7           B parameter.power 0.9622638
#> 8           C parameter.power 0.9622638
#> ============Evaluation Info============
#> • Alpha = 0.2 • Trials = 11 • Blocked = FALSE 
#> • Evaluating Model = ~A + B + C 
#> • Anticipated Coefficients = c(1, 1, 1, 1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#Evaluating a subset of the design (which changes the power due to a different number of
#degrees of freedom)
eval_design(design = optdesign, model= ~A + C, alpha = 0.2)
#>     parameter            type     power
#> 1 (Intercept)    effect.power 0.9659328
#> 2           A    effect.power 0.9659328
#> 3           C    effect.power 0.9659328
#> 4 (Intercept) parameter.power 0.9659328
#> 5           A parameter.power 0.9659328
#> 6           C parameter.power 0.9659328
#> ============Evaluation Info============
#> • Alpha = 0.2 • Trials = 11 • Blocked = FALSE 
#> • Evaluating Model = ~A + C 
#> • Anticipated Coefficients = c(1, 1, 1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#We do not have to input the model if it's the same as the model used
#During design generation. Here, we also use the default value for alpha (`0.05`)
eval_design(optdesign)
#>     parameter            type     power
#> 1 (Intercept)    effect.power 0.7991116
#> 2           A    effect.power 0.7991116
#> 3           B    effect.power 0.7991116
#> 4           C    effect.power 0.7991116
#> 5 (Intercept) parameter.power 0.7991116
#> 6           A parameter.power 0.7991116
#> 7           B parameter.power 0.7991116
#> 8           C parameter.power 0.7991116
#> ============Evaluation Info============
#> • Alpha = 0.05 • Trials = 11 • Blocked = FALSE 
#> • Evaluating Model = ~A + B + C 
#> • Anticipated Coefficients = c(1, 1, 1, 1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#Halving the signal-to-noise ratio by setting a different effectsize (default is 2):
eval_design(design = optdesign, model= ~A + B + C, alpha = 0.2, effectsize = 1)
#>     parameter            type     power
#> 1 (Intercept)    effect.power 0.6021367
#> 2           A    effect.power 0.6021367
#> 3           B    effect.power 0.6021367
#> 4           C    effect.power 0.6021367
#> 5 (Intercept) parameter.power 0.6021367
#> 6           A parameter.power 0.6021367
#> 7           B parameter.power 0.6021367
#> 8           C parameter.power 0.6021367
#> ============Evaluation Info============
#> • Alpha = 0.2 • Trials = 11 • Blocked = FALSE 
#> • Evaluating Model = ~A + B + C 
#> • Anticipated Coefficients = c(0.500, 0.500, 0.500, 0.500) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#With 3+ level categorical factors, the choice of anticipated coefficients directly changes the
#final power calculation. For the most conservative power calculation, that involves
#setting all anticipated coefficients in a factor to zero except for one. We can specify this
#option with the "conservative" argument.

factorialcoffee = expand.grid(cost = c(1, 2),
                              type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                              size = as.factor(c("Short", "Grande", "Venti")))

designcoffee = gen_design(factorialcoffee,
                         ~cost + size + type, trials = 29, optimality = "D", repeats = 100)

#Evaluate the design, with default anticipated coefficients (conservative is FALSE by default).
eval_design(designcoffee)
#>      parameter            type     power
#> 1  (Intercept)    effect.power 0.9990689
#> 2         cost    effect.power 0.9991530
#> 3         size    effect.power 0.9522717
#> 4         type    effect.power 0.9907584
#> 5  (Intercept) parameter.power 0.9990689
#> 6         cost parameter.power 0.9991530
#> 7        size1 parameter.power 0.9443775
#> 8        size2 parameter.power 0.9443775
#> 9        type1 parameter.power 0.8339422
#> 10       type2 parameter.power 0.8339422
#> 11       type3 parameter.power 0.8339422
#> ============Evaluation Info=============
#> • Alpha = 0.05 • Trials = 29 • Blocked = FALSE 
#> • Evaluating Model = ~cost + size + type 
#> • Anticipated Coefficients = c(1, 1, 1, -1, 1, -1, 1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#Evaluate the design, with conservative anticipated coefficients:
eval_design(designcoffee, conservative = TRUE)
#>      parameter            type     power
#> 1  (Intercept)    effect.power 0.9990689
#> 2         cost    effect.power 0.9991530
#> 3         size    effect.power 0.9522717
#> 4         type    effect.power 0.8300151
#> 5  (Intercept) parameter.power 0.9990689
#> 6         cost parameter.power 0.9991530
#> 7        size1 parameter.power 0.0500000
#> 8        size2 parameter.power 0.9443775
#> 9        type1 parameter.power 0.0500000
#> 10       type2 parameter.power 0.8339422
#> 11       type3 parameter.power 0.8339422
#> ============Evaluation Info=============
#> • Alpha = 0.05 • Trials = 29 • Blocked = FALSE 
#> • Evaluating Model = ~cost + size + type 
#> • Anticipated Coefficients = c(1, 1, 0, 1, 0, 1, -1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#which is the same as the following, but now explicitly entering the coefficients:
eval_design(designcoffee, anticoef = c(1, 1, 1, 0, 0, 1, 0))
#>      parameter            type     power
#> 1  (Intercept)    effect.power 0.9990689
#> 2         cost    effect.power 0.9991530
#> 3         size    effect.power 0.9654778
#> 4         type    effect.power 0.8478953
#> 5  (Intercept) parameter.power 0.9990689
#> 6         cost parameter.power 0.9991530
#> 7        size1 parameter.power 0.9443775
#> 8        size2 parameter.power 0.0500000
#> 9        type1 parameter.power 0.0500000
#> 10       type2 parameter.power 0.8339422
#> 11       type3 parameter.power 0.0500000
#> ============Evaluation Info=============
#> • Alpha = 0.05 • Trials = 29 • Blocked = FALSE 
#> • Evaluating Model = ~cost + size + type 
#> • Anticipated Coefficients = c(1, 1, 1, 0, 0, 1, 0) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#You can also evaluate the design with higher order effects, even if they were not
#used in design generation:
eval_design(designcoffee, model = ~cost + size + type + cost * type)
#>      parameter            type     power
#> 1  (Intercept)    effect.power 0.9989226
#> 2         cost    effect.power 0.9989944
#> 3         size    effect.power 0.9476843
#> 4         type    effect.power 0.9879203
#> 5    cost:type    effect.power 0.9859502
#> 6  (Intercept) parameter.power 0.9989226
#> 7         cost parameter.power 0.9989944
#> 8        size1 parameter.power 0.9361224
#> 9        size2 parameter.power 0.9361224
#> 10       type1 parameter.power 0.8215540
#> 11       type2 parameter.power 0.8215540
#> 12       type3 parameter.power 0.8215540
#> 13  cost:type1 parameter.power 0.8185524
#> 14  cost:type2 parameter.power 0.8185524
#> 15  cost:type3 parameter.power 0.8185524
#> ============Evaluation Info=============
#> • Alpha = 0.05 • Trials = 29 • Blocked = FALSE 
#> • Evaluating Model = ~cost + size + type + cost * type 
#> • Anticipated Coefficients = c(1, 1, 1, -1, 1, -1, 1, 1, -1, 1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 


#Generating and evaluating a split plot design:
splitfactorialcoffee = expand.grid(caffeine = c(1, -1),
                                  cost = c(1, 2),
                                  type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                                  size = as.factor(c("Short", "Grande", "Venti")))

coffeeblockdesign = gen_design(splitfactorialcoffee, ~caffeine, trials = 12)
coffeefinaldesign = gen_design(splitfactorialcoffee,
                              model = ~caffeine + cost + size + type, trials = 36,
                              splitplotdesign = coffeeblockdesign, blocksizes = 3)

#Evaluating design (blocking is automatically detected)
eval_design(coffeefinaldesign, 0.2, blocking = TRUE)
#>      parameter            type     power
#> 1  (Intercept)    effect.power 0.9453423
#> 2     caffeine    effect.power 0.9448040
#> 3         cost    effect.power 0.9999934
#> 4         size    effect.power 0.9990524
#> 5         type    effect.power 0.9998385
#> 6  (Intercept) parameter.power 0.9453423
#> 7     caffeine parameter.power 0.9448040
#> 8         cost parameter.power 0.9999934
#> 9        size1 parameter.power 0.9980496
#> 10       size2 parameter.power 0.9980496
#> 11       type1 parameter.power 0.9758443
#> 12       type2 parameter.power 0.9758443
#> 13       type3 parameter.power 0.9758443
#> ============Evaluation Info=============
#> • Alpha = 0.2 • Trials = 36 • Blocked = TRUE 
#> • Evaluating Model = ~caffeine + cost + size + type 
#> • Anticipated Coefficients = c(1, 1, 1, 1, -1, 1, -1, 1) 
#> • Number of Blocks = Level :  
#> • Variance Ratios  = Level 1: 1, Level 2: 1 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lmerTest::lmer(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#Manually turn blocking off to see completely randomized design power
eval_design(coffeefinaldesign, 0.2, blocking = FALSE)
#>      parameter            type     power
#> 1  (Intercept)    effect.power 0.9999981
#> 2     caffeine    effect.power 0.9999978
#> 3         cost    effect.power 0.9999978
#> 4         size    effect.power 0.9991258
#> 5         type    effect.power 0.9999460
#> 6  (Intercept) parameter.power 0.9999981
#> 7     caffeine parameter.power 0.9999978
#> 8         cost parameter.power 0.9999978
#> 9        size1 parameter.power 0.9981168
#> 10       size2 parameter.power 0.9981168
#> 11       type1 parameter.power 0.9828490
#> 12       type2 parameter.power 0.9828490
#> 13       type3 parameter.power 0.9828490
#> ============Evaluation Info=============
#> • Alpha = 0.2 • Trials = 36 • Blocked = FALSE 
#> • Evaluating Model = ~caffeine + cost + size + type 
#> • Anticipated Coefficients = c(1, 1, 1, 1, -1, 1, -1, 1) 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lm(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#We can also evaluate the design with a custom ratio between the whole plot error to
#the run-to-run error.
eval_design(coffeefinaldesign, 0.2, varianceratios = 2)
#>      parameter            type     power
#> 1  (Intercept)    effect.power 0.8158743
#> 2     caffeine    effect.power 0.8153104
#> 3         cost    effect.power 0.9999923
#> 4         size    effect.power 0.9990524
#> 5         type    effect.power 0.9998159
#> 6  (Intercept) parameter.power 0.8158743
#> 7     caffeine parameter.power 0.8153104
#> 8         cost parameter.power 0.9999923
#> 9        size1 parameter.power 0.9980496
#> 10       size2 parameter.power 0.9980496
#> 11       type1 parameter.power 0.9746799
#> 12       type2 parameter.power 0.9746799
#> 13       type3 parameter.power 0.9746799
#> ============Evaluation Info=============
#> • Alpha = 0.2 • Trials = 36 • Blocked = TRUE 
#> • Evaluating Model = ~caffeine + cost + size + type 
#> • Anticipated Coefficients = c(1, 1, 1, 1, -1, 1, -1, 1) 
#> • Number of Blocks = Level :  
#> • Variance Ratios  = Level 1: 2 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lmerTest::lmer(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 

#If the design was generated outside of skpr and thus the row names do not have the
#blocking structure encoded already, the user can add these manually. For a 12-run
#design with 4 blocks, here is a vector indicated the blocks:

blockcolumn = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)

#If we wanted to add this blocking structure to the design coffeeblockdesign, we would
#add a column with the format "Block1", "Block2", "Block3" ... and each one will be treated
#as a separate blocking layer.

coffeeblockdesign$Block1 = blockcolumn

#By default, skpr will throw out the blocking columns unless the user specifies `blocking = TRUE`.
eval_design(coffeeblockdesign, blocking=TRUE)
#>     parameter            type     power
#> 1 (Intercept)    effect.power 0.3326126
#> 2    caffeine    effect.power 0.6412616
#> 3 (Intercept) parameter.power 0.3326126
#> 4    caffeine parameter.power 0.6412616
#> ============Evaluation Info============
#> • Alpha = 0.05 • Trials = 12 • Blocked = TRUE 
#> • Evaluating Model = ~caffeine 
#> • Anticipated Coefficients = c(1, 1) 
#> • Number of Blocks = Level 1: 4 
#> • Variance Ratios  = Level 1: 1 
#> • Contrasts = `contr.sum` 
#> • Parameter Analysis Method = `lmerTest::lmer(...)` 
#> • Effect Analysis Method = `car::Anova(fit, type = "III")` 
```
