# Normalize Design

Normalizes the numeric columns in the design to -1 to 1. This is
important to do if your model has interaction or polynomial terms, as
these terms can introduce multi-collinearity and standardizing the
numeric columns can reduce this problem.

Normalizes the numeric columns in the design to -1 to 1. This is
important to do if your model has interaction or polynomial terms, as
these terms can introduce multi-collinearity and standardizing the
numeric columns can reduce this problem.

## Usage

``` r
normalize_design(design, augmented = NULL)

normalize_design(design, augmented = NULL)
```

## Arguments

- design:

  The design matrix.

- augmented:

  Default `NULL`. If

## Value

Normalized design matrix

Normalized run matrix

## Examples

``` r
#Normalize a design
if(skpr:::run_documentation()) {
cand_set = expand.grid(temp = c(100,300,500),
                      altitude = c(10000,20000),
                      offset = seq(-10,-5,by=1),
                      type = c("A","B", "C"))
design = gen_design(cand_set, ~., 24)

#Un-normalized design
design
}
#>    temp altitude offset type
#> 1   100    10000    -10    C
#> 2   500    20000     -5    A
#> 3   500    20000    -10    C
#> 4   100    10000     -5    C
#> 5   500    10000     -5    A
#> 6   500    20000    -10    B
#> 7   500    20000     -5    C
#> 8   100    20000     -5    A
#> 9   100    20000    -10    C
#> 10  500    20000     -5    B
#> 11  100    20000     -5    C
#> 12  500    10000    -10    C
#> 13  100    10000    -10    B
#> 14  100    10000     -5    A
#> 15  100    20000    -10    A
#> 16  500    10000     -5    C
#> 17  500    10000    -10    B
#> 18  500    20000    -10    A
#> 19  100    10000    -10    A
#> 20  100    20000     -5    B
#> 21  500    10000    -10    A
#> 22  100    20000    -10    B
#> 23  100    10000     -5    B
#> 24  500    10000     -5    B
if(skpr:::run_documentation()) {
#Normalized design
normalize_design(design)
}
#>    temp altitude offset type
#> 1    -1       -1     -1    C
#> 2     1        1      1    A
#> 3     1        1     -1    C
#> 4    -1       -1      1    C
#> 5     1       -1      1    A
#> 6     1        1     -1    B
#> 7     1        1      1    C
#> 8    -1        1      1    A
#> 9    -1        1     -1    C
#> 10    1        1      1    B
#> 11   -1        1      1    C
#> 12    1       -1     -1    C
#> 13   -1       -1     -1    B
#> 14   -1       -1      1    A
#> 15   -1        1     -1    A
#> 16    1       -1      1    C
#> 17    1       -1     -1    B
#> 18    1        1     -1    A
#> 19   -1       -1     -1    A
#> 20   -1        1      1    B
#> 21    1       -1     -1    A
#> 22   -1        1     -1    B
#> 23   -1       -1      1    B
#> 24    1       -1      1    B
```
