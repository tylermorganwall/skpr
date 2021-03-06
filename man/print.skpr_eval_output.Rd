% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{print.skpr_eval_output}
\alias{print.skpr_eval_output}
\title{Print evaluation information}
\usage{
\method{print}{skpr_eval_output}(x, ...)
}
\arguments{
\item{x}{The x of the evaluation functions in skpr}

\item{...}{Additional arguments.}
}
\description{
Prints design evaluation information below the data.frame of power values

Note: If options("skpr.ANSI") is `NULL` or `TRUE`, ANSI codes will be used during printing
to prettify the output. If this is `FALSE`, only ASCII will be used.
}
\examples{
#Generate/evaluate a design and print its information
factorialcoffee = expand.grid(cost = c(1, 2),
                              type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
                              size = as.factor(c("Short", "Grande", "Venti")))

designcoffee = gen_design(factorialcoffee,
                         ~cost + size + type, trials = 29, optimality = "D", repeats = 100)

eval_design(designcoffee)
}
