# Get optimality values

Returns a list of optimality values (or one value in particular).

Note: The choice of contrast will effect the `G` efficiency value, and
[`gen_design()`](https://tylermorganwall.github.io/skpr/reference/gen_design.md)
and
[`eval_design()`](https://tylermorganwall.github.io/skpr/reference/eval_design.md)
by default set different contrasts
([`contr.simplex()`](https://tylermorganwall.github.io/skpr/reference/contr.simplex.md)
vs `contr.sum`).

## Usage

``` r
get_optimality(output, optimality = NULL, calc_g = FALSE)
```

## Arguments

- output:

  The output of either
  [`gen_design()`](https://tylermorganwall.github.io/skpr/reference/gen_design.md),
  [`eval_design()`](https://tylermorganwall.github.io/skpr/reference/eval_design.md),
  or
  [`eval_design_mc()`](https://tylermorganwall.github.io/skpr/reference/eval_design_mc.md).

- optimality:

  Default `NULL`. Return just the specific optimality requested.

- calc_g:

  Default `FALSE`. Whether to calculate the g-efficiency.

## Value

A dataframe of optimality conditions. `D`, `A`, and `G` are efficiencies
(value is out of 100). `T` is the trace of the information matrix, `E`
is the minimum eigenvalue of the information matrix, `I` is the average
prediction variance, and `Alias` is the trace of the alias matrix.

## Examples

``` r
# We can extract the optimality of a design from either the output of `gen_design()`
# or the output of `eval_design()`

factorialcoffee = expand.grid(cost = c(1, 2),
                             type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                             size = as.factor(c("Short", "Grande", "Venti")))

designcoffee = gen_design(factorialcoffee, ~cost + size + type, trials = 29,
                         optimality = "D", repeats = 100)

#Extract a list of all attributes
get_optimality(designcoffee)
#>          D  I        A            G   T        E    Alias
#> 1 99.09996 NA 98.21557 Not Computed 203 22.79472 6.519097

#Get just one attribute
get_optimality(designcoffee,"D")
#>          D
#> 1 99.09996

# Extract from `eval_design()` output
power_output = eval_design(designcoffee, model = ~cost + size + type,
                          alpha = 0.05, detailedoutput = TRUE)

get_optimality(power_output)
#>          D         I        A            G   T        E    Alias
#> 1 99.09996 0.2224214 98.21557 Not Computed 203 22.79472 19.72452
```
