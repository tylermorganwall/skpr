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
#> 1   500    10000     -5    B
#> 2   500    10000    -10    C
#> 3   100    20000    -10    C
#> 4   100    10000     -5    C
#> 5   500    20000     -5    C
#> 6   500    10000     -5    A
#> 7   100    10000     -5    C
#> 8   500    10000    -10    C
#> 9   500    10000    -10    B
#> 10  100    10000    -10    A
#> 11  100    10000     -5    A
#> 12  500    20000     -5    C
#> 13  100    20000    -10    C
#> 14  500    20000     -5    B
#> 15  500    10000    -10    A
#> 16  100    10000     -5    B
#> 17  500    20000    -10    B
#> 18  500    20000     -5    A
#> 19  100    20000     -5    B
#> 20  100    20000    -10    B
#> 21  100    10000    -10    B
#> 22  500    20000    -10    A
#> 23  100    20000    -10    A
#> 24  100    20000     -5    A
if(skpr:::run_documentation()) {
#Normalized design
normalize_design(design)
}
#>    temp altitude offset type
#> 1     1       -1      1    B
#> 2     1       -1     -1    C
#> 3    -1        1     -1    C
#> 4    -1       -1      1    C
#> 5     1        1      1    C
#> 6     1       -1      1    A
#> 7    -1       -1      1    C
#> 8     1       -1     -1    C
#> 9     1       -1     -1    B
#> 10   -1       -1     -1    A
#> 11   -1       -1      1    A
#> 12    1        1      1    C
#> 13   -1        1     -1    C
#> 14    1        1      1    B
#> 15    1       -1     -1    A
#> 16   -1       -1      1    B
#> 17    1        1     -1    B
#> 18    1        1      1    A
#> 19   -1        1      1    B
#> 20   -1        1     -1    B
#> 21   -1       -1     -1    B
#> 22    1        1     -1    A
#> 23   -1        1     -1    A
#> 24   -1        1      1    A
```
