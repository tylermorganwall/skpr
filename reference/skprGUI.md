# Graphical User Interface for skpr

skprGUI provides a graphical user interface to skpr, within R Studio.

## Usage

``` r
skprGUI(
  browser = FALSE,
  return_app = FALSE,
  multiuser = FALSE,
  progress = TRUE
)
```

## Arguments

- browser:

  Default `FALSE`. Whether to open the application in an external
  browser.

- return_app:

  Default `FALSE`. If `TRUE`, this will return the shinyApp object.

- multiuser:

  Default `FALSE`. If `TRUE`, this will turn off and disable multicore
  functionality and enable non-blocking operation.

- progress:

  Default `TRUE`. Whether to include a progress bar in the application.
  Note: if `multiuser = TRUE`, progress bars are turned on by default.

## Examples

``` r
#Type `skprGUI()` to begin
```
