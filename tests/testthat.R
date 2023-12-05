Sys.setenv("R_TESTS" = "")

library(testthat)
library(shinytest2)

test_check("skpr")
