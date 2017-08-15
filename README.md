
Overview
--------

skpr is an open source design of experiments suite for generating and evaluating optimal designs in R. skpr provides a set of easy-to-use functions to generate optimal designs,

-   Generates and evaluates D, I, A, Alias, E, T, and G optimal designs.
-   Supports generation and evaluation of split/split-split/.../N-split plot designs.
-   Includes parametric and Monte Carlo power evaluation functions, and supports calculating power for censored responses.
-   Provides an extensible framework for the user to evaluate Monte Carlo power using their own libraries.
-   Includes a Shiny graphical user interface, skprGUI, that auto-generates working skpr code to improve ease-of-use and enhance reproducibility.

Functions
---------

-   `gen_design()` generates optimal designs from a candidate set, given a model and the desired number of runs.
-   `eval_design()` evaluates power parametrically for linear models, for normal and split-plot designs.
-   `eval_design_mc()` evaluates power with a Monte Carlo simulation, for linear and generalized linear models. This function also supports calculating power for split-plot designs using REML.
-   `eval_design_survival_mc()` evaluates power with a Monte Carlo simulation, allowing the user to specify a point at which the data is censored.
-   `eval_design_custom_mc()` allows the user to import their own libraries and use the Monte Carlo framework provided by skpr to calculate power.
-   `skprGUI()` and `skprGUIbrowser()` opens up the GUI in either R Studio and an external browser.

If addition, the package offers two functions to generate common plots related to designs:

-   `plot_correlations()` generates a color map of correlations between variables
-   `plot_fds()` generates the fraction of design space plot for a given design

skprGUI
-------

skprGUI provides an graphical user interface to access all of the main features of skpr. An interactive tutorial is provided to familiarize the user with the available functionality. Type `skprGUI()` or `skprGUIbrowser()` to begin.

<img src="man/figures/skprGUIcomp2.png" align="center"></img>

Usage
-----

``` r
library(skpr)

#Generate a candidate set of all potential design points to be considered in the experiment
#The hypothetical experiment here is determining what affects caffiene content in coffee
candidate_set = expand.grid(temp = c(80,90,100), 
                            type = c("Kona","Java"),
                            beansize = c("Large","Medium","Small"))

candidate_set
#>    temp type beansize
#> 1    80 Kona    Large
#> 2    90 Kona    Large
#> 3   100 Kona    Large
#> 4    80 Java    Large
#> 5    90 Java    Large
#> 6   100 Java    Large
#> 7    80 Kona   Medium
#> 8    90 Kona   Medium
#> 9   100 Kona   Medium
#> 10   80 Java   Medium
#> 11   90 Java   Medium
#> 12  100 Java   Medium
#> 13   80 Kona    Small
#> 14   90 Kona    Small
#> 15  100 Kona    Small
#> 16   80 Java    Small
#> 17   90 Java    Small
#> 18  100 Java    Small

#Generate the design (default D-optimal)
design = gen_design(candidateset = candidate_set, 
                    model = ~temp + type + beansize,
                    trials=12)

design
#>    temp type beansize
#> 1    80 Kona    Large
#> 2   100 Java    Large
#> 3    80 Kona    Small
#> 4    80 Java   Medium
#> 5   100 Kona   Medium
#> 6   100 Java    Small
#> 7    80 Java   Medium
#> 8    80 Kona    Small
#> 9    80 Java    Large
#> 10  100 Kona   Medium
#> 11  100 Java    Small
#> 12  100 Kona    Large

#Evaluate power for the design with an allowable type-I error of 5%
eval_design(RunMatrix = design,
            model = ~temp + type + beansize,
            alpha=0.05)
#>     parameter            type     power
#> 1 (Intercept)    effect.power 0.8424665
#> 2        temp    effect.power 0.8424665
#> 3        type    effect.power 0.8424665
#> 4    beansize    effect.power 0.5165386
#> 5 (Intercept) parameter.power 0.8424665
#> 6        temp parameter.power 0.8424665
#> 7       type1 parameter.power 0.8424665
#> 8   beansize1 parameter.power 0.5593966
#> 9   beansize2 parameter.power 0.5593966

#Evaluate power for the design using a Monte Carlo simulation. 
#Here, we set the delta/sigma (the signal-to-noise ratio) to 1.5.
eval_design_mc(RunMatrix = design,
               model = ~temp + type + beansize,
               alpha=0.05,
               delta=1.5)
#>     parameter               type power
#> 1 (Intercept) parameter.power.mc 0.619
#> 2        temp parameter.power.mc 0.605
#> 3       type1 parameter.power.mc 0.630
#> 4   beansize1 parameter.power.mc 0.355
#> 5   beansize2 parameter.power.mc 0.352

#Evaluate power for the design using a Monte Carlo simulation, for a non-normal response. 
#Here, we also increase the number of simululations to improve the precision of the results.
eval_design_mc(RunMatrix = design,
               model = ~temp + type + beansize,
               nsim=5000,
               glmfamily = "poisson",
               alpha=0.05,
               delta=log(8/2))
#> Warning: glm.fit: fitted rates numerically 0 occurred
#>     parameter               type  power
#> 1 (Intercept) parameter.power.mc 0.6282
#> 2        temp parameter.power.mc 0.9646
#> 3       type1 parameter.power.mc 0.9648
#> 4   beansize1 parameter.power.mc 0.8954
#> 5   beansize2 parameter.power.mc 0.6622

#skpr was designed to operate with the pipe (%>%) in mind. 
#Here is an example of an entire design of experiments analysis in three lines:

library(dplyr)

expand.grid(temp = c(80,90,100), type = c("Kona","Java"), beansize = c("Large","Medium","Small")) %>%
  gen_design(model = ~temp + type + beansize + beansize:type + I(temp^2), trials=24, optimality="I") %>%
  eval_design_mc(model = ~temp + type + beansize + beansize:type + I(temp^2), alpha=0.05)
#>         parameter               type power
#> 1     (Intercept) parameter.power.mc 0.899
#> 2            temp parameter.power.mc 0.895
#> 3           type1 parameter.power.mc 0.995
#> 4       beansize1 parameter.power.mc 0.894
#> 5       beansize2 parameter.power.mc 0.893
#> 6       I(temp^2) parameter.power.mc 0.630
#> 7 type1:beansize1 parameter.power.mc 0.893
#> 8 type1:beansize2 parameter.power.mc 0.898
```
