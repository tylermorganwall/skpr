library(skpr)

options("skpr_progress" = FALSE)
on.exit(options("skpr_progress" = NULL), add = TRUE)
skprGUI(return_app = TRUE)

