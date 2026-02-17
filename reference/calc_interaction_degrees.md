# Calculate Interaction Degrees of Freedom

Calculate interaction degrees of freedom for split-plot designs

## Usage

``` r
calc_interaction_degrees(
  design,
  model,
  contrast,
  nointercept,
  split_layers,
  split_degrees,
  split_plot_structure
)
```

## Arguments

- design:

  The design matrix

- model:

  The model

- contrast:

  The contrast

- split_layers:

  The layer of split plots for each main effect term

- split_degrees:

  The number of degrees of freedom for each main effect term

## Value

Degrees of freedom vector
