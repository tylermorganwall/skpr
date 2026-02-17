# Converts dot operator to terms

Converts the dot operator `.` in a formula to the linear terms in the
model. Includes interactions (e.g. .\*.)

## Usage

``` r
convert_model_dots(design, model, splitplotdesign = NULL)
```

## Arguments

- design:

  The design

- model:

  Base model

- splitplotdesign:

  split plot design data.frame

## Value

New model with dot operator replaced
