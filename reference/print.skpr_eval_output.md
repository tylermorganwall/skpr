# Print evaluation information

Prints design evaluation information below the data.frame of power
values

Note: If options("skpr.ANSI") is `NULL` or `TRUE`, ANSI codes will be
used during printing to prettify the output. If this is `FALSE`, only
ASCII will be used.

## Usage

``` r
# S3 method for class 'skpr_eval_output'
print(x, ...)
```

## Arguments

- x:

  The x of the evaluation functions in skpr

- ...:

  Additional arguments.

## Examples

``` r
#Generate/evaluate a design and print its information
factorialcoffee = expand.grid(cost = c(1, 2),
                              type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                              size = as.factor(c("Short", "Grande", "Venti")))

designcoffee = gen_design(factorialcoffee,
                         ~cost + size + type, trials = 29, optimality = "D", repeats = 100)

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
```
