# Create Code Pane Reactives

Create Code Pane Reactives

## Usage

``` r
create_code_pane_reactives(input, update, candidatesetall, any_htc_fn)
```

## Arguments

- input:

  Shiny input object.

- update:

  Reactive dependency used to trigger recomputation.

- candidatesetall:

  Reactive that returns the full candidate set list.

- any_htc_fn:

  Function returning whether any factors are hard-to-change.

## Value

List of reactives required exclusively by the code pane.
