#'@title Graphical User Interface for skpr
#'
#'@description skprGUI provides a graphical user interface to skpr, within R Studio.
#'
#'@param browser Default `FALSE`. Whether to open the application in an external browser.
#'@param return_app Default `FALSE`. If `TRUE`, this will return the shinyApp object.
#'@param multiuser Default `FALSE`. If `TRUE`, this will turn off and disable multicore functionality and enable non-blocking operation.
#'@param progress Default `TRUE`. Whether to include a progress bar in the application. Note: if `multiuser = TRUE`, progress
#'bars are turned on by default.
#'
#'@import doRNG
#'@export
#'@examples
#'#Type `skprGUI()` to begin
#'
# nocov start
skprGUI = function(
  browser = FALSE,
  return_app = FALSE,
  multiuser = FALSE,
  progress = TRUE
) {
  check_for_suggest_packages(c(
    "shiny",
    "shinythemes",
    "shinyjs",
    "gt",
    "rintrojs",
    "ggplot2"
  ))
  skpr_progress = getOption("skpr_progress", progress)

  oplan = future::plan()
  original_future_call = deparse(
    attr(oplan, "call", exact = TRUE),
    width.cutoff = 500L
  )
  if (original_future_call != "NULL") {
    if (skpr_system_setup_env$has_multicore_support) {
      message_string = 'plan("multicore", workers = number_of_cores-1)'
    } else {
      message_string = 'plan("multisession", workers = number_of_cores-1)'
    }
    message(sprintf(
      "Using user-defined {future} plan() `%s` instead of {skpr}'s default plan (for this computer) of `%s`",
      original_future_call,
      message_string
    ))
  } else {
    numbercores = getOption(
      "cores",
      default = getOption(
        "Ncpus",
        default = max(c(1, future::availableCores() - 1))
      )
    )
    if (skpr_system_setup_env$has_multicore_support && !multiuser) {
      plan("multicore", workers = numbercores, .call = NULL)
    } else {
      plan("multisession", workers = numbercores, .call = NULL)
    }
  }
  b64 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFUAAAAnCAYAAAB+HwSQAAAACXBIWXMAAC4jAAAuIwF4pT92AAAAB3RJTUUH5AEKEAsxvdBAYQAAAAd0RVh0QXV0aG9yAKmuzEgAAAAMdEVYdERlc2NyaXB0aW9uABMJISMAAAAKdEVYdENvcHlyaWdodACsD8w6AAAADnRFWHRDcmVhdGlvbiB0aW1lADX3DwkAAAAJdEVYdFNvZnR3YXJlAF1w/zoAAAALdEVYdERpc2NsYWltZXIAt8C0jwAAAAh0RVh0V2FybmluZwDAG+aHAAAAB3RFWHRTb3VyY2UA9f+D6wAAAAh0RVh0Q29tbWVudAD2zJa/AAAABnRFWHRUaXRsZQCo7tInAAAFW0lEQVRoge2aa6hVRRTHf+vejq/Uq6WZVj6SIEoiSkQpIorMCCSUwkQjtYdpBL3UrJAQLC0lzMgyqC/RBQ2KEAn6UFEISpJBaZpW+KCXmY/ypl7/fZh98nicmb3POfvaRu4fBvaZtWbN2uvMrL1mrQF4BhBwPNAE/Aj0lwTwUNLXHhmTpR0DDgN7gI1Aa6LLbcCFkmi0ASVgc8r7ld/xF2BgLvMmkz+ZCPa1DUCvKmUnJkYNjWm0tQFrgbE5GLYv8GXKfNuAAXkYtNKoV0YmfMWjaG/gUAcatbK9S7JLGjDsmylzfJaXQSXRhEN3wih5+mL8eWMSsN7MrqhnsJmVgJtS2EaZ2eX1yPehKZ2lLpzI0GrBcOBjMxtahy6TgGEpPF2B+XXI9uKcvARV4AtgGmApfF2APsAlwDXAWOCqCP9AYI2ZjZZ0PIsiZmbAvCy8wN1mtlDS9oz8YSQ+ZyRhf7PS46MGEPap6xrwfXcCuyK6CJhfg7wJKbKq29t5+tQ80VzvQEmrgTHAtxG2uWbWL6PIWrf0ZDMbXuOY09BRPrVuSNoNjAcOBlh6A9PT5JjZOOBaD2kXMCcwrEQOvrVwRgWQtAN4PsIyOYOYpwP9qyW9COwI0KeYWdqHLYpCGjXB68CBAG1EbJua2Q3A9R7SUeDV5Hl5YHgX4KmsSvpQWKNK2g98GiA349/aZYRW6RpJO5Pnt4DfA3z3mNmQdC39KKxRE6yP0LyHATMbiQvPfFhSfpB0CFgV4OtK9lDsNBTdqN9HaBcH+kOr9CNJm6v6VgBHAvz3mtngmHIhFN2o+yK0luoOMxuBixx8eKG6Q9JeXG7Bh27A3DQFfSi6UY9GaL6cxDz877RB0icBOcsIH5unmVloRwRRdKP6DFfGscofSTRwV4B3SaAfSd8A6wLk7oRj2iCKbtTzI7TqcGsO/j/hK0nvpcyzAJcf9mGGmQ1KGX8KOiKhkicujdD2lh+SLTo1wLfWzC4j/q6HgK24vHI1euCS+I/GVT2Joht1dIS2teL5McI53icIRwRlnMCVVUK438wWS/o5RQ5Q4O1vZr2AGwPkE7gSCWbWH7gvIqprhumacCepEM7F/TmZUFij4pIm5wVoW3B1JYBHgF5nQJ8HzeyCLIyFNKqZXQQ8G2FplSQzawFmRfiyVCCyViR6Ao9n0b9wPjXJlX5A+Mv/F66QBzAT/2o+DNyCOzykVSAqIeABwlt9ppm9JOm3uJRiZf5vB7ZHdBGwIOHtgYsAfDwvN6BDb+CPyPyL0mR0xErtmRToYitEuJiyBRgMXA3cCoxKkb0JWJQ8T8fVrarRBizNrm6VYtJBM1tB2P3MMrOlksJH6A5Yqe3AP7gjZqzVehljNzAkmb8E/BDgW9VojQnnev6M6LLwTNeoyuFJKaXVMvd24GZJPyW/pwBDPXxtRI6kWZGswtciLA+bWd8QsfxibREBxzx9oXRZR6AVGCPpu4q+UGb+c+VRYnZ4g/DRtQ+x6CRZ7rG7VJuAlqrtMTHCn0fz3qXCBfLLI+O2AYPyKDMDd2TQcypg1WMNmA0sJhyjlYCvgXGS9pvZZFxyt9GPnHBHwyO45MjexCgbcXebdlYyJ9vtfeA64O+IrruB8ZK21KuYmc0GniN+ymrC2eAdSTNOGQ/0S5QJLXVwCds9ktqTl+uB3y3UgrJR2ySluhMza8bdZjmSjI3p+qukmEtLm6s/zqBp79gMNMuV1U+OT5Z6J3KEUduJoxN+mKT/3Kfh/GW3/0+fswbLJK0EZ9TO/Z8PDgDDJe1rIvwl7URtaMFFDBjOqGfyZvTZjHZgmOFu13X61HzQDHz4LwBKRJdWuineAAAAAElFTkSuQmCC"
  if (skpr_progress && !multiuser) {
    progressr::handlers(global = TRUE)
  }
  #Load Shiny functions from namespace (due to shiny being Suggests)
  checkboxInput = shiny::checkboxInput
  tagList = shiny::tagList
  reactiveVal = shiny::reactiveVal
  sliderInput = shiny::sliderInput
  incProgress = shiny::incProgress
  htmlOutput = shiny::htmlOutput
  tableOutput = shiny::tableOutput
  icon = shiny::icon
  bookmarkButton = shiny::bookmarkButton
  mainPanel = shiny::mainPanel
  radioButtons = shiny::radioButtons
  req = shiny::req
  updateNavbarPage = shiny::updateNavbarPage
  fluidPage = shiny::fluidPage
  HTML = shiny::HTML
  sidebarLayout = shiny::sidebarLayout
  sidebarPanel = shiny::sidebarPanel
  tags = shiny::tags
  hr = shiny::hr
  fluidRow = shiny::fluidRow
  column = shiny::column
  actionButton = shiny::actionButton
  tabsetPanel = shiny::tabsetPanel
  tabPanel = shiny::tabPanel
  numericInput = shiny::numericInput
  textInput = shiny::textInput
  uiOutput = shiny::uiOutput
  br = shiny::br
  selectInput = shiny::selectInput
  conditionalPanel = shiny::conditionalPanel
  sliderInput = shiny::sliderInput
  checkboxInput = shiny::checkboxInput
  h1 = shiny::h1
  h2 = shiny::h2
  h3 = shiny::h3
  plotOutput = shiny::plotOutput
  reactive = shiny::reactive
  isolate = shiny::isolate
  showNotification = shiny::showNotification
  updateSelectInput = shiny::updateSelectInput
  withProgress = shiny::withProgress
  bindEvent = shiny::bindEvent
  renderPlot = shiny::renderPlot
  renderUI = shiny::renderUI
  renderText = shiny::renderText
  reactiveValues = shiny::reactiveValues
  reactiveValuesToList = shiny::reactiveValuesToList
  debounce = shiny::debounce
  observeEvent = shiny::observeEvent
  outputOptions = shiny::outputOptions
  runGadget = shiny::runGadget
  shinyApp = shiny::shinyApp
  browserViewer = shiny::browserViewer
  dialogViewer = shiny::dialogViewer
  invalidateLater = shiny::invalidateLater
  observe = shiny::observe
  Progress = shiny::Progress
  ggplot = ggplot2::ggplot
  geom_point = ggplot2::geom_point
  geom_errorbar = ggplot2::geom_errorbar
  theme = ggplot2::theme
  theme_light = ggplot2::theme_light
  element_blank = ggplot2::element_blank
  element_text = ggplot2::element_text
  scale_x_continuous = ggplot2::scale_x_continuous
  scale_y_continuous = ggplot2::scale_y_continuous
  scale_x_discrete = ggplot2::scale_x_discrete
  scale_y_discrete = ggplot2::scale_y_discrete
  scale_color_manual = ggplot2::scale_color_manual
  labs = ggplot2::labs
  aes = ggplot2::aes
  geom_hline = ggplot2::geom_hline
  geom_histogram = ggplot2::geom_histogram
  geom_vline = ggplot2::geom_vline

  if (!getOption("in_skpr_test_environment", FALSE)) {
    est_plot_width = "auto"
    pvalue_plot_width = "auto"
    fdsplot_width = "auto"
    optimalsearch_plot_width = "auto"
  } else {
    est_plot_width = 922
    pvalue_plot_width = 952
    fdsplot_width = 446
    optimalsearch_plot_width = 446
  }

  panelstyle = "background-color: rgba(86, 96, 133, 0.3);
  border-radius: 15px;
  -webkit-box-shadow: inset 0px 0px 10px 4px rgba(41, 49, 83, 0.42);
  box-shadow: inset 0px 0px 10px 2px rgba(41, 49, 83, 0.42);
  padding-top: 15px;
  padding-bottom: 10px;
  color: rgb(255, 255, 255);
  border: 0px;"

  ui = function(request) {
    fluidPage(
      theme = shinythemes::shinytheme("yeti"),
      shinyjs::useShinyjs(),
      rintrojs::introjsUI(),
      HTML(
        "<style> table {font-size: 14px;}
                   .btn2 {
                   color: #fff;
                   border-color: rgba(46, 109, 164, 0);
                   background: linear-gradient(to bottom, rgb(64, 108, 221) 0%, rgb(107, 167, 223) 100%);
                   border-radius: 11px;
                   -webkit-box-shadow: 0px 2px 0px 0px rgb(59, 76, 145);
                   box-shadow: 0px 2px 0px 0px rgb(59, 76, 145);
                   }
                   .btn2:hover {
                   color: #fff;
                   border-color: rgb(48, 163, 43);
                   background: linear-gradient(to bottom, rgb(78, 206, 114) 0%, rgb(71, 146, 95) 100%);
                   border-radius: 11px;
                   -webkit-box-shadow: 0px 2px 0px 0px rgb(59, 76, 145);
                   box-shadow: 0px 2px 0px 0px rgb(59, 76, 145);
                   }
                    .btn2.disabled, .btn2.disabled:hover, .btn2.disabled:active, .btn2.disabled:focus {
                      color: #fff;
                      border-color: rgba(46, 109, 164, 0);
                      background: linear-gradient(to bottom, rgb(64, 108, 221) 0%, rgb(107, 167, 223) 100%);
                      border-radius: 11px;
                      -webkit-box-shadow: 0px 0px 0px 0px rgb(59, 76, 145);
                      box-shadow: 0px 0px 0px 0px rgb(59, 76, 145);
                      margin-top: 2px;
                      margin-bottom: -2px;
                      }
                   .btn2:active {
                   color: #fff;
                   border-color: rgb(64, 108, 221);
                   background-color: rgb(71, 146, 95);
                   border-radius: 11px;
                   -webkit-box-shadow: 0px 0px 0px 0px rgb(59, 76, 145);
                   box-shadow: 0px 0px 0px 0px rgb(59, 76, 145);
                   margin-top: 2px;
                   margin-bottom: -2px;
                   }
                   .btn2:focus {
                   color: #fff;
                   }
                   .nav-tabs>li>a {
                   background-color: rgb(71, 81, 156);
                   border: 1px solid rgb(71, 81, 156);
                   color: #ffffff;
                   transition: background-color 0.2s ease-in-out, color 0.2s ease-in-out, border 0.2s ease-in-out;
                   }
                   .nav-tabs>li>a:active {
                   background-color: rgb(184, 255, 181);
                   border: 1px solid rgb(184, 255, 181);
                   color: #000000;
                   transition: background-color 0.2s ease-in-out, color 0.2s ease-in-out, border 0.2s ease-in-out;
                   }
                   .nav-tabs>li>a:focus {
                   background-color: rgb(184, 255, 181);
                   border: 1px solid rgb(184, 255, 181);
                   color: #000000;
                   transition: background-color 0.2s ease-in-out, color 0.2s ease-in-out, border 0.2s ease-in-out;
                   }

                   .nav-tabs>li>a:hover {
                   background-color: rgb(184, 255, 181);
                   border: 1px solid rgb(184, 255, 181);
                   color: #000000;
                   transition: background-color 0.2s ease-in-out, color 0.2s ease-in-out, border 0.2s ease-in-out;
                   }
                   .nav-tabs>li.active>a {
                   background-color: rgb(184, 255, 181);
                   border: 1px solid rgb(184, 255, 181);
                   color: #000000;
                   transition: background-color 0.2s ease-in-out, color 0.2s ease-in-out, border 0.2s ease-in-out;
                   }
                   .nav-tabs>li.active>a:focus {
                   background-color: rgb(184, 255, 181);
                   border: 1px solid rgb(184, 255, 181);
                   color: #000000;
                   transition: background-color 0.2s ease-in-out, color 0.2s ease-in-out, border 0.2s ease-in-out;
                   }
                   .nav-tabs>li.active>a:hover {
                   background-color: rgb(184, 255, 181);
                   border: 1px solid rgb(184, 255, 181);
                   color: #000000;
                   transition: background-color 0.2s ease-in-out, color 0.2s ease-in-out, border 0.2s ease-in-out;
                   }
                   .selectize-control:hover {
                   -webkit-box-shadow: 0px 5px 10px 0px rgba(255, 255, 255, 0.30);
                   box-shadow: 0px 5px 10px 0px rgba(255, 255, 255, 0.30);
                   transition: box-shadow 0.2s ease-in-out;
                   }


                   .form-control:hover {
                   -webkit-box-shadow: 0px 5px 10px 0px rgba(0, 0, 0, 0.30);
                   box-shadow: 0px 5px 10px 0px rgba(255, 255, 255, 0.41);
                   transition: box-shadow 0.2s ease-in-out;
                   }
                   .well .nav-tabs>li {
                   width: 33%;
                   }
                   .well h1 {
                   text-shadow: 0px 0px 30px rgb(255, 255, 255);
                   }
                   .control-label, label {font-size: 16px; font-weight: 600;}
                   .well h3 {margin-top: 0px;}
                   .well .nav {margin-bottom: 6px; margin-top: 12px;margin-left: -20px;margin-right: -26px; border-bottom: 1px solid transparent;text-align: center;}
                   hr {margin-top: 8px; margin-bottom: 0px;}
                   .well hr {
                   margin-top: -12px;
                   margin-bottom: 13px;
                   margin-left: -20px;
                   margin-right: -20px;
                   border-top: 1px solid rgb(0, 0, 0);
                   }
                   @media (min-width: 768px) and (max-width: 1150px) { #designtext {font-size: 0;} }
                   @media (min-width: 768px) { #evalbutton {float: right;} .btn2 { width: 100%; } }
                   @media (max-width: 767px) { #evalbutton {margin-top: 10px;} .btn2{ width: 100%;} }
                   .irs-grid-text {color: rgb(0, 0, 0);}</style>"
      ),
      sidebarLayout(
        sidebarPanel(
          tags$style(
            ".well {background-color:#a1b0da;
                                        border: 1px solid #a1b0da;
                                        border-radius: 13px;
                                        -webkit-box-shadow: 0px 0px 10px 5px rgba(0, 0, 0, 0.15);
                                        box-shadow: 0px 0px 10px 5px rgba(0, 0, 0, 0.15);}"
          ),
          HTML(
            "<h1 style='margin-top: 0px;'>skpr<strong style='color: black;'>GUI</strong></h1>"
          ),
          hr(),
          rintrojs::introBox(
            fluidRow(
              column(
                width = 6,
                actionButton(
                  "submitbutton",
                  HTML("<strong>Generate <br>Design</strong>"),
                  class = "btn2"
                )
              ),
              column(
                width = 6,
                actionButton(
                  "evalbutton",
                  HTML("<strong>Evaluate <br>Design</strong>"),
                  class = "btn2"
                )
              )
            ),
            data.step = 1,
            data.intro = "<h3><center>Welcome to skpr!</h3></center> This tutorial will walk you through all of the features of the GUI and teach you how to create and analyze an experimental design. All features seen in the GUI can be easily recreated in the console, and skpr provides the full script used to do that, based on your inputs. Additional advanced capabilities not available in the GUI can be accessed via the code. <b>Let's get started!</b> <br><br>Click these buttons to generate a new design, or re-run a new design evaluation with updated parameters."
          ),
          tabsetPanel(
            tabPanel(
              "Basic",
              rintrojs::introBox(
                numericInput(inputId = "trials", 12, label = "Trials"),
                data.step = 2,
                data.intro = "This is the number of runs in the experiment."
              ),
              rintrojs::introBox(
                textInput(inputId = "model", "~.", label = "Model"),
                data.step = 3,
                data.intro = "This is the model. <br><br> <b>~.</b> produces a linear model for all terms with no interactions. <br><br> Interactions can be added with the colon operator: <br><br> <b>~X1 + X2 + X1:X2</b> <br><br> and quadratic effects with an I() (as in India): <br><br><b>~X1 + X2 + I(X1^2)</b>."
              ),

              uiOutput("block_panel"),
              rintrojs::introBox(
                numericInput(
                  inputId = "numberfactors",
                  min = 1,
                  max = NA,
                  1,
                  label = "Number of Factors"
                ),
                data.step = 4,
                data.intro = "This is the number of factors in the experiment. "
              ),
              br(),
              rintrojs::introBox(
                generate_factor_input_panel(1),
                data.step = 5,
                data.intro = "This pane allows you to change the factor type, specify categorical and discrete numeric levels, and make factors hard-to-change. If numeric, specify the highest and lowest values and the number of breaks between. If categorical or discrete numeric, specify levels separated by commas."
              ),
              uiOutput("additional_factors")
            ),
            tabPanel(
              "Advanced",
              rintrojs::introBox(
                selectInput(
                  inputId = "optimality",
                  choices = c("D", "I", "A", "Alias", "G", "E", "T"),
                  label = "Optimality"
                ),
                data.step = 6,
                data.intro = "Change the optimality criterion. If Alias-optimal selected, additional Alias-optimal specific options (minimum D-optimality and Alias-interaction level) will become available to change."
              ),
              rintrojs::introBox(
                numericInput(inputId = "repeats", 20, label = "Repeats"),
                data.step = 7,
                data.intro = "Changes the depth of the optimal design search. Increasing this will increase the probability that an optimal design is found."
              ),
              rintrojs::introBox(
                numericInput(
                  inputId = "varianceratio",
                  1,
                  label = "Variance Ratio"
                ),
                data.step = 8,
                data.intro = "The ratio of the variance between whole plots and subplots for split-plot designs."
              ),
              conditionalPanel(
                condition = "input.optimality == \'Alias\'",
                numericInput(
                  inputId = "aliaspower",
                  min = 2,
                  value = 2,
                  label = "Alias Optimal Interaction Level"
                ),
                sliderInput(
                  inputId = "mindopt",
                  min = 0,
                  max = 1,
                  value = 0.8,
                  label = "Minimum D Optimality"
                )
              ),
              rintrojs::introBox(
                checkboxInput(
                  inputId = "setseed",
                  label = "Set Random Number Generator Seed",
                  value = FALSE
                ),
                data.step = 9,
                data.intro = "Set the random seed for both design generation and evaluation. This allows for completely reproducible designs and Monte Carlo simulations."
              ),
              conditionalPanel(
                condition = "input.setseed",
                numericInput(inputId = "seed", 1, label = "Random Seed")
              ),
              rintrojs::introBox(
                checkboxInput(
                  inputId = "parallel",
                  label = ifelse(
                    !multiuser,
                    "Parallel Search",
                    "Parallel Search (disabled on multiuser app)"
                  ),
                  value = FALSE
                ),
                data.step = 10,
                data.intro = "Use all available cores to compute design. Only set to true if the design search is taking >10 seconds to finish. Otherwise, the overhead in setting up the parallel computation outweighs the speed gains."
              ),
              rintrojs::introBox(
                checkboxInput(
                  inputId = "splitanalyzable",
                  label = "Include Blocking Columns in Run Matrix",
                  value = TRUE
                ),
                data.step = 11,
                data.intro = "Convert row structure to blocking columns. This is required for analyzing the split-plot structure using REML."
              ),
              rintrojs::introBox(
                checkboxInput(
                  inputId = "detailedoutput",
                  label = "Detailed Output",
                  value = FALSE
                ),
                data.step = 12,
                data.intro = "Outputs a tidy data frame of additional design information, including anticipated coefficients and design size."
              ),
              rintrojs::introBox(
                checkboxInput(
                  inputId = "advanceddiagnostics",
                  label = "Advanced Design Diagnostics",
                  value = TRUE
                ),
                data.step = 13,
                data.intro = "Outputs additional information about the optimal search and advanced Monte Carlo information. This includes a list of all available optimal criteria, a plot of the computed optimal values during the search (useful for determining if the repeats argument should be increased), and a histogram of p-values for each parameter in Monte Carlo simulations."
              ),
              selectInput(
                inputId = "colorchoice",
                choices = c(
                  "Default" = "D",
                  "Magma" = "A",
                  "Inferno" = "B",
                  "Plasma" = "C",
                  "None" = "none"
                ),
                label = "Color"
              )
            ),
            tabPanel(
              "Power",
              rintrojs::introBox(
                rintrojs::introBox(
                  rintrojs::introBox(
                    radioButtons(
                      inputId = "evaltype",
                      label = "Model Type",
                      choiceNames = c(
                        "Linear Model",
                        "Generalized Linear Model",
                        "Survival Model"
                      ),
                      choiceValues = c("lm", "glm", "surv")
                    ),
                    data.step = 14,
                    data.intro = "Change the type of analysis. Linear model calculates power with parametric assumptions, while the Generalized Linear Model and Survival Model both calculate power using a Monte Carlo approach."
                  ),
                  data.step = 18,
                  data.intro = "Changing the evaluation type to a GLM Monte Carlo reveals several additional controls."
                ),
                data.step = 22,
                data.intro = "Survival analysis Monte Carlo power generation. This simulates data according to the design, and then censors the data if it is above or below a user defined threshold. This simulation is performed with the survreg package."
              ),
              rintrojs::introBox(
                sliderInput(
                  inputId = "alpha",
                  min = 0,
                  max = 1,
                  value = 0.05,
                  label = "Alpha"
                ),
                data.step = 15,
                data.intro = "Specify the acceptable Type-I error (false positive rate)"
              ),
              sliderInput(
                inputId = "desired_power",
                min = 0,
                max = 1,
                value = 0.80,
                label = "Desired Power"
              ),
              conditionalPanel(
                condition = "input.evaltype == \'lm\' || (input.evaltype == \'glm\' && input.glmfamily == \'gaussian\') || (input.evaltype == \'surv\' && (input.distribution == \'gaussian\' || input.distribution == \'lognormal\'))",
                rintrojs::introBox(
                  numericInput(
                    inputId = "snr",
                    value = 2,
                    step = 0.1,
                    label = "SNR"
                  ),
                  data.step = 16,
                  data.intro = "Signal-to-noise ratio for linear models."
                )
              ),
              conditionalPanel(
                condition = "input.evaltype == \'glm\' && input.glmfamily == \'poisson\'",
                fluidRow(
                  column(
                    width = 6,
                    numericInput(
                      inputId = "poislow",
                      "Low # of Events:",
                      min = 0,
                      value = 1
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      inputId = "poishigh",
                      "High # of Events:",
                      min = 0,
                      value = 2
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "(input.evaltype == \'glm\' && input.glmfamily == \'exponential\') || (input.evaltype == \'surv\' && input.distribution == \'exponential\')",
                fluidRow(
                  column(
                    width = 6,
                    numericInput(
                      inputId = "explow",
                      "Low Mean:",
                      min = 0,
                      value = 1
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      inputId = "exphigh",
                      "High Mean:",
                      min = 0,
                      value = 2
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.evaltype == \'glm\' && input.glmfamily == \'binomial\'",
                sliderInput(
                  inputId = "binomialprobs",
                  "Binomial Probabilities:",
                  min = 0,
                  max = 1,
                  value = c(0.4, 0.6)
                ),
                checkboxInput(
                  inputId = "firth_correction",
                  "Use Firth Correction",
                  value = TRUE
                )
              ),
              conditionalPanel(
                condition = "input.evaltype == \'glm\'",
                checkboxInput(
                  inputId = "adjust_alpha",
                  "Adjust for Type-I Error Inflation",
                  value = FALSE
                )
              ),
              rintrojs::introBox(
                conditionalPanel(
                  condition = "input.evaltype == \'lm\'",
                  checkboxInput(
                    inputId = "conservative",
                    label = "Conservative Power",
                    value = FALSE
                  )
                ),
                data.step = 17,
                data.intro = "Calculates conservative effect power for 3+ level categorical factors. Calculates power once, and then sets the anticipated coefficient corresponding to the highest power level in each factor to zero. The effect power for those factors then show the most conservative power estimate."
              ),
              conditionalPanel(
                condition = "input.evaltype == \'glm\'",
                rintrojs::introBox(
                  numericInput(
                    inputId = "nsim",
                    value = 1000,
                    label = "Number of Simulations"
                  ),
                  data.step = 19,
                  data.intro = "The number of Monte Carlo simulations to run. More simulations will result in a more precise power estimation."
                ),
                rintrojs::introBox(
                  selectInput(
                    inputId = "glmfamily",
                    choices = c(
                      "gaussian",
                      "binomial",
                      "poisson",
                      "exponential"
                    ),
                    label = "GLM Family"
                  ),
                  data.step = 20,
                  data.intro = "The distributional family used in the generalized linear model. If binomial, an additional slider will appear allowing you to change the desired upper and lower probability bounds. This automatically calculates the anticipated coefficients that correspond to that probability range."
                ),
                rintrojs::introBox(
                  checkboxInput(
                    inputId = "parallel_eval_glm",
                    label = ifelse(
                      !multiuser,
                      "Parallel Evaluation",
                      "Parallel Evaluation (disabled on multiuser app)"
                    ),
                    value = FALSE
                  ),
                  data.step = 21,
                  data.intro = "Turn on multicore support for evaluation. Should only be used if the calculation is taking >10s to complete. Otherwise, the overhead in setting up the parallel computation outweighs the speed gains."
                )
              ),
              conditionalPanel(
                condition = "input.evaltype == \'surv\'",
                numericInput(
                  inputId = "nsim_surv",
                  value = 1000,
                  label = "Number of Simulations"
                ),
                selectInput(
                  inputId = "distribution",
                  choices = c("gaussian", "lognormal", "exponential"),
                  label = "Distribution"
                ),
                rintrojs::introBox(
                  numericInput(
                    inputId = "censorpoint",
                    value = NA,
                    label = "Censor Point"
                  ),
                  data.step = 23,
                  data.intro = "The value after (if right censored) or before (if left censored) data will be censored. The default is no censoring."
                ),
                rintrojs::introBox(
                  selectInput(
                    inputId = "censortype",
                    choices = c("right", "left"),
                    label = "Censoring Type"
                  ),
                  data.step = 24,
                  data.intro = "The type of censoring."
                ),
                checkboxInput(
                  inputId = "parallel_eval_surv",
                  label = ifelse(
                    !multiuser,
                    "Parallel Evaluation",
                    "Parallel Evaluation (disabled on multiuser app)"
                  ),
                  value = FALSE
                )
              ),
              checkboxInput(
                inputId = "colorblind",
                label = "Colorblind Palette",
                value = FALSE
              )
            )
          )
        ),
        mainPanel(
          fluidRow(
            column(width = 4, h1("Results")),
            column(width = 2),
            column(
              width = 2,
              rintrojs::introBox(
                bookmarkButton(
                  label = "Save State",
                  title = "Generates a URL that encodes the current state of the application for easy sharing and saving of analyses. Paste this URL into a  (possible changing the port and address if locally different) to restore the state of the application. Be sure to set a random seed before bookmarking to recover the same results."
                ),
                class = "bookmark",
                data.step = 33,
                data.intro = "Generates a URL that encodes the current state of the application for easy sharing and saving of analyses. Paste this URL into a  (possible changing the port and address if locally different) to restore the state of the application. Be sure to set a random seed before bookmarking to recover the same results."
              )
            ),
            column(
              width = 2,
              actionButton(
                inputId = "tutorial",
                "Tutorial",
                icon = icon("question-circle")
              )
            ),
            column(
              width = 2,
              HTML(
                paste0(
                  "<div style='float:right; margin-top: 25px;'><img src=",
                  b64,
                  "></img></div>"
                )
              )
            ),
            tags$style(
              type = "text/css",
              "#tutorial {margin-top: 25px;} .bookmark {margin-top: 25px;}"
            )
          ),
          tabsetPanel(
            id = "results_panels",
            tabPanel(
              "Design",
              value = "design",
              checkboxInput(
                inputId = "orderdesign",
                label = "Order Design",
                value = FALSE
              ),
              rintrojs::introBox(
                tableOutput(outputId = "runmatrix"),
                data.step = 25,
                data.intro = "The generated optimal design. If hard-to-change factors are present, there will be an additional blocking column specifying the block number. Here, we have generated a design with three factors and 12 runs."
              ),
              hr()
            ),
            tabPanel(
              "Design Evaluation",
              value = "eval",
              rintrojs::introBox(
                fluidRow(
                  column(
                    width = 12,
                    h2("Power Results"),
                    conditionalPanel(
                      condition = "input.evaltype == \'lm\'",
                      gt::gt_output(outputId = "powerresults")
                    ),
                    rintrojs::introBox(
                      conditionalPanel(
                        condition = "input.evaltype == \'glm\'",
                        gt::gt_output(outputId = "powerresultsglm")
                      ),
                      data.step = 27,
                      data.intro = "The power of the design. Output is a tidy data frame of the power and the type of evaluation for each parameter. If the evaluation type is parametric and there are 3+ level categorical factors, effect power will also be shown. Here, we have our GLM simulated power estimation."
                    ),
                    conditionalPanel(
                      condition = "input.evaltype == \'surv\'",
                      gt::gt_output(outputId = "powerresultssurv")
                    )
                  )
                ),
                data.step = 26,
                data.intro = "This page shows the calculated/simulated power, as well as other design diagnostics. (results may take a second to appear)"
              ),
              hr(),
              fluidRow(
                align = "center",
                column(
                  width = 6,
                  h3("Correlation Map"),
                  rintrojs::introBox(
                    conditionalPanel(
                      "output.displayed_design_number_factors != 1",
                      plotOutput(outputId = "aliasplot", width = fdsplot_width)
                    ),
                    data.step = 28,
                    data.intro = "Correlation map of the design. This shows the correlation structure between main effects and their interactions. Ideal correlation structures will be diagonal (top left to bottom right). Alias-optimal designs minimize the elements of this matrix that correspond to a main effects term interacting with an interaction term."
                  ),
                  conditionalPanel(
                    "output.displayed_design_number_factors == 1",
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    HTML(
                      "<font color=#898989> One Parameter: <br>No Correlation Map</font>"
                    )
                  )
                ),
                column(
                  width = 6,
                  h3("Fraction of Design Space"),
                  rintrojs::introBox(
                    plotOutput(outputId = "fdsplot", width = fdsplot_width),
                    data.step = 29,
                    data.intro = "Fraction of design space plot. The horizontal line corresponds to the average prediction variance for the design."
                  )
                )
              ),
              conditionalPanel(
                condition = "input.evaltype == \'glm\'",
                fluidRow(
                  hr(),
                  column(
                    width = 12,
                    h3("Simulated Response Estimates"),
                    rintrojs::introBox(
                      plotOutput(
                        outputId = "responsehistogram",
                        width = est_plot_width
                      ),
                      data.step = 30,
                      data.intro = "Distribution of response estimates for Monte Carlo simulations. For a given design and distributional family, this plot shows the model's estimates of the overall response of the experiment (red) with the actual values on top (blue). "
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.evaltype == \'surv\'",
                fluidRow(
                  hr(),
                  column(
                    width = 12,
                    h3("Simulated Response Estimates"),
                    plotOutput(
                      outputId = "responsehistogramsurv",
                      width = est_plot_width
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.evaltype == \'glm\'",
                fluidRow(
                  hr(),
                  column(
                    width = 12,
                    h3("Simulated Estimates"),
                    rintrojs::introBox(
                      plotOutput(
                        outputId = "parameterestimates",
                        width = est_plot_width
                      ),
                      data.step = 31,
                      data.intro = "Individual parameter estimates for each of the design factors. The 95% confidence intervals are extracted from the actual simulated values."
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.evaltype == \'surv\'",
                fluidRow(
                  hr(),
                  column(
                    width = 12,
                    h3("Simulated Estimates"),
                    plotOutput(
                      outputId = "parameterestimatessurv",
                      width = est_plot_width
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.advanceddiagnostics",
                hr(),
                fluidRow(
                  align = "left",
                  uiOutput(outputId = "optimality_results"),
                  column(
                    width = 6,
                    h3("Optimal Search Values"),
                    plotOutput(
                      outputId = "optimalsearch",
                      width = optimalsearch_plot_width
                    )
                  ),
                  hr(),
                  fluidRow(
                    conditionalPanel(
                      condition = "input.evaltype != \'lm\'",
                      column(
                        width = 12,
                        h3("Simulated P-Values"),
                        plotOutput(
                          outputId = "simulatedpvalues",
                          width = pvalue_plot_width
                        )
                      )
                    )
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  h3("Number of Terms in Design"),
                  shiny::textOutput(
                    outputId = "displayed_design_number_factors"
                  ),
                )
              )
            ),
            tabPanel(
              "Generating Code",
              value = "code",
              rintrojs::introBox(
                htmlOutput(outputId = "code"),
                data.step = 32,
                data.intro = "The R code used to generate the design and evaluate power. This section is updated in real time as the user changes the inputs. Copy and paste this code at the end to easily save, distribute, and reproduce your results. This also provides an easy code template to automate more complex design searches not built in to the GUI. Also included is the code showing how to analyze the experiment once the data has been collected, for all supported types of analyses. "
              )
            )
          )
        )
      )
    )
  }

  server = function(input, output, session) {
    inc_progress_session = function(amount = 0.1, message = NULL, detail = NULL)
      incProgress(amount, message, detail, session)

    session_id = session$token
    progress_file_name = tempfile(pattern = sprintf("progress_%s_", session_id))
    runmatrix_file_name = tempfile(
      pattern = sprintf("runmatrix_%s_", session_id),
      fileext = ".Rds"
    )
    power_file_name = tempfile(
      pattern = sprintf("power_%s_", session_id),
      fileext = ".Rds"
    )

    tempdir_runmatrix = tempdir()

    #### Multiuser Progress Bar ############
    multiuser_progress_bar_updater = function(...) {
      if (file.exists(progress_file_name)) {
        number = file.info(progress_file_name)$size - 1
        if (number < 0) {
          number = 0
        }
        ## This clears the file of any existing information
        close(file(progress_file_name, open = "w"))
      } else {
        number = 0
      }
      stringtowrite = as.character(paste0(rep(0, number + 1), collapse = ""))
      prog_file = file(progress_file_name, "wb")
      writeBin(stringtowrite, prog_file)
      close(prog_file)
    }

    #### Progress Bar Environment ############
    prog_env = new.env()
    prog_env$previouspercent = 0
    prog_env$percentdone = 0
    prog_env$need_to_initialize_progress_bar = TRUE
    prog_env$progress_type = ""

    #### Multiuser Reactive Values ############
    multiuser_rv = reactiveValues()
    multiuser_rv$runmatrix = NA
    multiuser_rv$powerresultsglm = NA
    multiuser_rv$powerresultssurv = NA
    ## Add stop and reset buttons ##
    #### Multiuser Observer ############

    multiuser_helper_runmatrix = observe(
      {
        if (multiuser) {
          invalidateLater(500)
          runmatrix_future_resolved = future::resolved(runmatrix_container())
          isolate({
            #Handle design generation progress bars
            if (
              exists("progress", envir = prog_env) &&
                prog_env$progress_type == "gen"
            ) {
              if (
                file.exists(runmatrix_file_name) &&
                  runmatrix_future_resolved
              ) {
                tempval = tryCatch(
                  {
                    readRDS(file.path(runmatrix_file_name))
                  },
                  error = function(e) "",
                  warning = function(w) ""
                )
                if (inherits(tempval, "data.frame")) {
                  multiuser_rv$runmatrix = tempval
                  tryCatch(
                    {
                      file.remove(runmatrix_file_name)
                    },
                    error = function(e) "",
                    warning = function(w) ""
                  )
                }
              }
              if (runmatrix_future_resolved) {
                shinyjs::enable("evalbutton")
                shinyjs::enable("submitbutton")
                current_design_valid(TRUE)
                if (is.function(prog_env$progress$close)) {
                  prog_env$progress$close()
                  rm("progress", envir = prog_env)
                  prog_env$need_to_initialize_progress_bar = TRUE
                  prog_env$progress_type = ""
                  close(file(progress_file_name, open = "w"))
                }
              } else {
                if (prog_env$need_to_initialize_progress_bar) {
                  prog_env$progress$set(
                    message = "Searching for designs...",
                    value = 0
                  )
                  prog_env$need_to_initialize_progress_bar = FALSE
                }
                if (input$repeats > 200) {
                  sims = 200
                } else {
                  sims = input$repeats
                }
                if (isblocking()) {
                  sims = sims * 2
                }
                #Increment via percent
                prog_env$percentdone = (file.info(progress_file_name)$size -
                  1) /
                  sims
                prog_env$progress$inc(
                  prog_env$percentdone - prog_env$previouspercent
                )
                prog_env$previouspercent = prog_env$percentdone
              }
            }

            #Handle GLM power
            # if(exists("progress", envir = prog_env) &&
            #    prog_env$progress_type == "glm") {
            #   if(file.exists(power_file_name) &&
            #      power_results_glm_resolved) {
            #     tempval = tryCatch({
            #       readRDS(file.path(power_file_name))
            #     }, error = function(e) "", warning = function(w) "")
            #     if (inherits(tempval, "data.frame")) {
            #       multiuser_rv$powerresultsglm  = tempval
            #       tryCatch({
            #         file.remove(power_file_name)
            #       }, error = function(e) "", warning = function(w) "")
            #     }
            #   }
            #   if(power_results_glm_resolved) {
            #     shinyjs::enable("evalbutton")
            #     shinyjs::enable("submitbutton")
            #     current_design_valid(TRUE)
            #     if(is.function(prog_env$progress$close)) {
            #       prog_env$progress$close()
            #       rm(progress, envir = prog_env)
            #       prog_env$need_to_initialize_progress_bar = TRUE
            #       prog_env$progress_type = ""
            #       close(file(progress_file_name, open="w"))
            #     }
            #   } else {
            #     if (prog_env$need_to_initialize_progress_bar) {
            #       prog_env$progress$set(message = "Calculating power...", value = 0)
            #       prog_env$need_to_initialize_progress_bar = FALSE
            #     }
            #     if (input$nsim > 200) {
            #       sims = 200
            #     } else {
            #       sims = input$nsim
            #     }
            #     if(isblocking()) {
            #       sims = sims * 2
            #     }
            #     #Increment via percent
            #     prog_env$percentdone = (file.info(progress_file_name)$size - 1) / sims
            #     prog_env$progress$inc(prog_env$percentdone - prog_env$previouspercent)
            #     prog_env$previouspercent = prog_env$percentdone
            #   }
            # }

            #Handle survival power
            # if(exists("progress", envir = prog_env) &&
            #    prog_env$progress_type == "surv") {
            #   if(file.exists(power_file_name) &&
            #      power_results_surv_resolved) {
            #     tempval = tryCatch({
            #       readRDS(file.path(power_file_name))
            #     }, error = function(e) "", warning = function(w) "")
            #     if (inherits(tempval, "data.frame")) {
            #       multiuser_rv$powerresultssurv  = tempval
            #       tryCatch({
            #         file.remove(power_file_name)
            #       }, error = function(e) "", warning = function(w) "")
            #     }
            #   }
            #   if(power_results_surv_resolved) {
            #     shinyjs::enable("evalbutton")
            #     shinyjs::enable("submitbutton")
            #     current_design_valid(TRUE)
            #     if(is.function(prog_env$progress$close)) {
            #       prog_env$progress$close()
            #       rm(progress, envir = prog_env)
            #       prog_env$need_to_initialize_progress_bar = TRUE
            #       prog_env$progress_type = ""
            #       close(file(progress_file_name, open="w"))
            #     }
            #   } else {
            #     if (prog_env$need_to_initialize_progress_bar) {
            #       prog_env$progress$set(message = "Calculating power...", value = 0)
            #       prog_env$need_to_initialize_progress_bar = FALSE
            #     }
            #     if (input$nsim_surv > 200) {
            #       sims = 200
            #     } else {
            #       sims = input$nsim_surv
            #     }
            #     # if(isblocking()) {
            #     #   sims = sims * 2
            #     # }
            #     #Increment via percent
            #     prog_env$percentdone = (file.info(progress_file_name)$size - 1) / sims
            #     prog_env$progress$inc(prog_env$percentdone - prog_env$previouspercent)
            #     prog_env$previouspercent = prog_env$percentdone
            #   }
            # }

            #Handle GLM power simulation progress bars
            # if(exists("progress", envir = prog_env) &&
            #    prog_env$progress_type == "glm") {
            #   power_results_glm_resolved = promise_fulfilled_rejected(powerresultsglm())
            #   if(power_results_glm_resolved) {
            #     shinyjs::enable("evalbutton")
            #     shinyjs::enable("submitbutton")
            #     current_design_valid(TRUE)
            #     if(is.function(prog_env$progress$close)) {
            #       prog_env$progress$close()
            #       rm(progress, envir = prog_env)
            #       prog_env$need_to_initialize_progress_bar = TRUE
            #       prog_env$progress_type = ""
            #     }
            #   } else {
            #
            #   }
            # }
            #
            # #Handle survival power simulation progress bars
            # if(exists("progress", envir = prog_env) &&
            #    prog_env$progress_type == "surv") {
            #   power_results_surv_resolved = promise_fulfilled_rejected(powerresultssurv())
            #   if(power_results_surv_resolved) {
            #     shinyjs::enable("evalbutton")
            #     shinyjs::enable("submitbutton")
            #     current_design_valid(TRUE)
            #     if(is.function(prog_env$progress$close)) {
            #       prog_env$progress$close()
            #       rm(progress, envir = prog_env)
            #       prog_env$need_to_initialize_progress_bar = TRUE
            #       prog_env$progress_type = ""
            #     }
            #   }
            # }
            # print("11")
            # print(current_design_valid())
            # ()
            # print(class(powerresultsglm))
            # print(class(powerresultsglm()))
            # print(powerresultsglm())

            # power_results_glm_resolved = promise_fulfilled_rejected(powerresultsglm())
            # print("111")
            # power_results_glm_resolved = TRUE
            # power_results_surv_resolved = TRUE
            # power_results_surv_resolved = promise_fulfilled_rejected(powerresultssurv())
            # print("1111")

            # if(evaluationtype() == "glm") {
            #   power_results_resolved = power_results_glm_resolved
            # } else if(evaluationtype() == "surv") {
            #   power_results_resolved = power_results_surv_resolved
            # } else {
            #   power_results_resolved = TRUE
            # }
            # print("22")
            # if(power_results_resolved && current_design_valid()) {
            #   shinyjs::enable("evalbutton")
            #   shinyjs::enable("submitbutton")
            # }
            # print("33")
            # ### Now deal with multi-user progress bars
            #
            # # If the current progress bar type matches the future and the future has resolved
            # # but never it happened quickly enough that we never updated the progress bar once,
            # # set need to initialize to false.
            # print("44")
            # if(exists("progress", envir = prog_env) &&
            #    ((runmatrix_future_resolved && prog_env$progress_type == "gen") ||
            #     (power_results_resolved    && prog_env$progress_type == "pow"))) {
            #   print("2")
            #   prog_env$need_to_initialize_progress_bar = FALSE
            # }
            # print("55")
            # #Here we handle the progress bar for design generation
            # if (!runmatrix_future_resolved &&
            #     exists("progress", envir = prog_env) &&
            #     prog_env$progress_type == "gen") {
            #   print("3")
            #
            #   ## This just changes the message from "setting up"
            #   if (prog_env$need_to_initialize_progress_bar) {
            #     prog_env$progress$set(message = "Searching for designs...", value = 0)
            #     prog_env$need_to_initialize_progress_bar = FALSE
            #   }
            #
            #   #Here, we have a maximum of 200 increments on the progress bar. (x2 if split plot)
            #   if (input$repeats > 200) {
            #     sims = 200
            #   } else {
            #     sims = input$repeats
            #   }
            #   if(isblocking()) {
            #     sims = sims * 2
            #   }
            #
            #   #Increment via percent
            #   prog_env$percentdone = (file.info(progress_file_name)$size - 1) / sims
            #   prog_env$progress$inc(prog_env$percentdone - prog_env$previouspercent)
            #   prog_env$previouspercent = prog_env$percentdone
            # }
            # print("66")
            # #Here we handle the progress bar for power calculations
            # if (!power_results_resolved &&
            #     exists("progress", envir = prog_env) &&
            #     prog_env$progress_type == "pow") {
            #   print("4")
            #   ## This just changes the message from "setting up"
            #   if (prog_env$need_to_initialize_progress_bar) {
            #     prog_env$progress$set(message = "Calculating power...", value = 0)
            #     prog_env$need_to_initialize_progress_bar =  FALSE
            #   }
            #
            #   #Here, we have a maximum of 200 increments on the progress bar. (x2 if adjusting alpha)
            #   if(evaluationtype() != "surv") {
            #     if (input$nsim > 200) {
            #       sims = 200
            #     } else {
            #       sims = input$nsim
            #     }
            #   } else {
            #     if (input$nsim_surv > 200) {
            #       sims = 200
            #     } else {
            #       sims = input$nsim_surv
            #     }
            #   }
            #
            #   if(input$adjust_alpha) {
            #     sims = sims * 2
            #   }
            #
            #   #Increment via percent
            #   prog_env$percentdone = (file.info(progress_file_name)$size - 1) / sims
            #   prog_env$progress$inc(prog_env$percentdone - prog_env$previouspercent)
            #   prog_env$previouspercent = prog_env$percentdone
            # }
            # # If the search has ended and the future has resolved, delete the progress bar
            # # and clear the file tracking the number of iterations.
            # print(c(runmatrix_future_resolved, exists("progress", envir = prog_env),
            #         prog_env$progress_type == "gen", !prog_env$need_to_initialize_progress_bar))
            # if (runmatrix_future_resolved &&
            #     exists("progress", envir = prog_env) &&
            #     prog_env$progress_type == "gen" &&
            #     !prog_env$need_to_initialize_progress_bar) {
            #   print("5")
            #   # shinyjs::enable("submitbutton")
            #   # shinyjs::enable("evalbutton")
            #   # if(file.exists(progress_file_name)) {
            #   #   file.remove(progress_file_name)
            #   # }
            #   close(file(progress_file_name, open="w"))
            #   if(is.function(prog_env$progress$close)) {
            #     prog_env$progress$close()
            #     rm(progress, envir = prog_env)
            #     prog_env$need_to_initialize_progress_bar = TRUE
            #   }
            # }
            # if (power_results_resolved &&
            #     exists("progress", envir = prog_env) &&
            #     prog_env$progress_type == "pow" &&
            #     !prog_env$need_to_initialize_progress_bar) {
            #   print("6")
            #   # shinyjs::enable("submitbutton")
            #   # shinyjs::enable("evalbutton")
            #   # if(file.exists(progress_file_name)) {
            #   #   file.remove(progress_file_name)
            #   # }
            #   close(file(progress_file_name, open="w"))
            #   if(is.function(prog_env$progress$close)) {
            #     prog_env$progress$close()
            #     rm(progress, envir = prog_env)
            #     prog_env$need_to_initialize_progress_bar = TRUE
            #   }
            # }
          })
        }
      },
      label = "multiuser_observer_runmatrix",
      priority = 2
    )

    multiuser_helper_glm = observe(
      {
        req(powerresultsglm_container(), cancelOutput = TRUE)
        if (multiuser && evaluationtype() == "glm") {
          invalidateLater(500)
          power_results_glm_resolved = future::resolved(powerresultsglm_container())
          isolate({
            #Handle GLM power
            if (
              exists("progress", envir = prog_env) &&
                prog_env$progress_type == "glm"
            ) {
              if (
                file.exists(power_file_name) &&
                  power_results_glm_resolved
              ) {
                tempval = tryCatch(
                  {
                    readRDS(file.path(power_file_name))
                  },
                  error = function(e) "",
                  warning = function(w) ""
                )
                if (inherits(tempval, "data.frame")) {
                  multiuser_rv$powerresultsglm = tempval
                  tryCatch(
                    {
                      file.remove(power_file_name)
                    },
                    error = function(e) "",
                    warning = function(w) ""
                  )
                }
              }
              if (power_results_glm_resolved) {
                shinyjs::enable("evalbutton")
                shinyjs::enable("submitbutton")
                current_design_valid(TRUE)
                if (is.function(prog_env$progress$close)) {
                  prog_env$progress$close()
                  rm("progress", envir = prog_env)
                  prog_env$need_to_initialize_progress_bar = TRUE
                  prog_env$progress_type = ""
                  close(file(progress_file_name, open = "w"))
                }
              } else {
                if (prog_env$need_to_initialize_progress_bar) {
                  prog_env$progress$set(
                    message = "Calculating power...",
                    value = 0
                  )
                  prog_env$need_to_initialize_progress_bar = FALSE
                }
                if (input$nsim > 200) {
                  sims = 200
                } else {
                  sims = input$nsim
                }
                if (isblocking()) {
                  sims = sims * 2
                }
                #Increment via percent
                prog_env$percentdone = (file.info(progress_file_name)$size -
                  1) /
                  sims
                prog_env$progress$inc(
                  prog_env$percentdone - prog_env$previouspercent
                )
                prog_env$previouspercent = prog_env$percentdone
              }
            }
          })
        }
      },
      label = "multiuser_observer_glm",
      priority = 1
    )

    multiuser_helper_surv = observe(
      {
        req(powerresultssurv_container(), cancelOutput = TRUE)
        if (multiuser && evaluationtype() == "surv") {
          invalidateLater(500)
          power_results_surv_resolved = future::resolved(powerresultssurv_container())
          isolate({
            #Handle survival power
            if (
              exists("progress", envir = prog_env) &&
                prog_env$progress_type == "surv"
            ) {
              if (
                file.exists(power_file_name) &&
                  power_results_surv_resolved
              ) {
                tempval = tryCatch(
                  {
                    readRDS(file.path(power_file_name))
                  },
                  error = function(e) "",
                  warning = function(w) ""
                )
                if (inherits(tempval, "data.frame")) {
                  multiuser_rv$powerresultssurv = tempval
                  tryCatch(
                    {
                      file.remove(power_file_name)
                    },
                    error = function(e) "",
                    warning = function(w) ""
                  )
                }
              }
              if (power_results_surv_resolved) {
                shinyjs::enable("evalbutton")
                shinyjs::enable("submitbutton")
                current_design_valid(TRUE)
                if (is.function(prog_env$progress$close)) {
                  prog_env$progress$close()
                  rm("progress", envir = prog_env)
                  prog_env$need_to_initialize_progress_bar = TRUE
                  prog_env$progress_type = ""
                  close(file(progress_file_name, open = "w"))
                }
              } else {
                if (prog_env$need_to_initialize_progress_bar) {
                  prog_env$progress$set(
                    message = "Calculating power...",
                    value = 0
                  )
                  prog_env$need_to_initialize_progress_bar = FALSE
                }
                if (input$nsim_surv > 200) {
                  sims = 200
                } else {
                  sims = input$nsim_surv
                }
                #Increment via percent
                prog_env$percentdone = (file.info(progress_file_name)$size -
                  1) /
                  sims
                prog_env$progress$inc(
                  prog_env$percentdone - prog_env$previouspercent
                )
                prog_env$previouspercent = prog_env$percentdone
              }
            }
          })
        }
      },
      label = "multiuser_observer_surv",
      priority = 0
    )

    if (multiuser) {
      shinyjs::disable("parallel")
      shinyjs::disable("parallel_eval_glm")
      shinyjs::disable("parallel_eval_surv")
    }

    ##### Unify Run Matrix ###########
    runmatrix = reactive(
      {
        if (!multiuser) {
          runmatrix_container()
        } else {
          multiuser_rv$runmatrix
        }
      },
      label = "runmatrix_unifier"
    )

    powerresultsglm = reactive(
      {
        if (!multiuser) {
          powerresultsglm_container()
        } else {
          multiuser_rv$powerresultsglm
        }
      },
      label = "power_glm_unifier"
    )

    powerresultssurv = reactive(
      {
        if (!multiuser) {
          powerresultssurv_container()
        } else {
          multiuser_rv$powerresultssurv
        }
      },
      label = "power_surv_unifier"
    )

    inputlist_htc = reactive({
      input$submitbutton
      inputlist1 = list()
      for (i in seq_len(input$numberfactors)) {
        factorname_n = sprintf("factorname%i", i)
        factortype_n = sprintf("factortype%i", i)
        numericlow_n = sprintf("numericlow%i", i)
        numerichigh_n = sprintf("numerichigh%i", i)
        numericlength_n = sprintf("numericlength%i", i)
        disclevels_n = sprintf("disclevels%i", i)
        levels_n = sprintf("levels%i", i)
        blockdepth_n = sprintf("blockdepth%i", i)

        if (input[[numericlow_n]] == "htc") {
          if (input[[factortype_n]] == "numeric") {
            inputlist1[[input[[factorname_n]]]] = seq(
              input[[numericlow_n]],
              input[[numerichigh_n]],
              length.out = input[[numericlength_n]]
            )
          }
          if (input[[factortype_n]] == "discnum") {
            inputlist1[[input[[factorname_n]]]] = as.numeric(strsplit(
              gsub(" ", "", input[[disclevels_n]], fixed = TRUE),
              split = ","
            )[[1]])
          }
          if (input[[factortype_n]] == "cat") {
            inputlist1[[input[[factorname_n]]]] = strsplit(
              gsub(" ", "", input[[levels_n]], fixed = TRUE),
              split = ","
            )[[1]]
          }
        }
      }
      inputlist1
    })

    inputlist_htctext = reactive({
      input$submitbutton
      inputlist1 = list()
      for (i in seq_len(input$numberfactors)) {
        factorname_n = sprintf("factorname%i", i)
        factortype_n = sprintf("factortype%i", i)
        numericlow_n = sprintf("numericlow%i", i)
        numerichigh_n = sprintf("numerichigh%i", i)
        numericlength_n = sprintf("numericlength%i", i)
        disclevels_n = sprintf("disclevels%i", i)
        levels_n = sprintf("levels%i", i)
        blockdepth_n = sprintf("blockdepth%i", i)

        if (input[[blockdepth_n]] == "htc") {
          if (input[[factortype_n]] == "numeric") {
            inputlist1[[input[[factorname_n]]]] = seq(
              input[[numericlow_n]],
              input[[numerichigh_n]],
              length.out = input[[numericlength_n]]
            )
          }
          if (input[[factortype_n]] == "discnum") {
            inputlist1[[input[[factorname_n]]]] = as.numeric(strsplit(
              gsub(" ", "", input[[disclevels_n]], fixed = TRUE),
              split = ","
            )[[1]])
          }
          if (input[[factortype_n]] == "cat") {
            inputlist1[[input[[factorname_n]]]] = strsplit(
              gsub(" ", "", input[[levels_n]], fixed = TRUE),
              split = ","
            )[[1]]
          }
        }
      }
      inputlist1
    })

    inputlist = reactive({
      inputlist1 = list()
      for (i in seq_len(input$numberfactors)) {
        factorname_n = sprintf("factorname%i", i)
        factortype_n = sprintf("factortype%i", i)
        numericlow_n = sprintf("numericlow%i", i)
        numerichigh_n = sprintf("numerichigh%i", i)
        numericlength_n = sprintf("numericlength%i", i)
        disclevels_n = sprintf("disclevels%i", i)
        levels_n = sprintf("levels%i", i)
        blockdepth_n = sprintf("blockdepth%i", i)

        if (input[[blockdepth_n]] == "etc") {
          if (input[[factortype_n]] == "numeric") {
            inputlist1[[input[[factorname_n]]]] = seq(
              input[[numericlow_n]],
              input[[numerichigh_n]],
              length.out = input[[numericlength_n]]
            )
          }
          if (input[[factortype_n]] == "discnum") {
            inputlist1[[input[[factorname_n]]]] = as.numeric(strsplit(
              gsub(" ", "", input[[disclevels_n]], fixed = TRUE),
              split = ","
            )[[1]])
          }
          if (input[[factortype_n]] == "cat") {
            inputlist1[[input[[factorname_n]]]] = strsplit(
              gsub(" ", "", input[[levels_n]], fixed = TRUE),
              split = ","
            )[[1]]
          }
        }
      }
      inputlist1
    })

    candidatesetall = reactive({
      candidateset1 = list()
      for (i in seq_len(input$numberfactors)) {
        factorname_n = sprintf("factorname%i", i)
        factortype_n = sprintf("factortype%i", i)
        numericlow_n = sprintf("numericlow%i", i)
        numerichigh_n = sprintf("numerichigh%i", i)
        numericlength_n = sprintf("numericlength%i", i)
        disclevels_n = sprintf("disclevels%i", i)
        levels_n = sprintf("levels%i", i)
        blockdepth_n = sprintf("blockdepth%i", i)
        req(input[[factortype_n]], cancelOutput = TRUE)
        if (input[[factortype_n]] == "numeric") {
          candidateset1[[input[[factorname_n]]]] = seq(
            input[[numericlow_n]],
            input[[numerichigh_n]],
            length.out = input[[numericlength_n]]
          )
        }
        if (input[[factortype_n]] == "discnum") {
          candidateset1[[input[[factorname_n]]]] = as.numeric(strsplit(
            gsub(" ", "", input[[disclevels_n]], fixed = TRUE),
            split = ","
          )[[1]])
        }
        if (input[[factortype_n]] == "cat") {
          candidateset1[[input[[factorname_n]]]] = strsplit(
            gsub(" ", "", input[[levels_n]], fixed = TRUE),
            split = ","
          )[[1]]
        }
      }
      candidateset1
    })

    inputstring = reactive({
      req(update, cancelOutput = TRUE)
      updatevector = list()
      finalstring = c()
      commacount = input$numberfactors - 1
      for (i in seq_len(input$numberfactors)) {
        factorname_n = sprintf("factorname%i", i)
        factortype_n = sprintf("factortype%i", i)
        numericlow_n = sprintf("numericlow%i", i)
        numerichigh_n = sprintf("numerichigh%i", i)
        numericlength_n = sprintf("numericlength%i", i)
        disclevels_n = sprintf("disclevels%i", i)
        levels_n = sprintf("levels%i", i)
        blockdepth_n = sprintf("blockdepth%i", i)
        finalstring = c(finalstring, input[[factorname_n]], " = ")
        req(input[[factortype_n]], cancelOutput = TRUE)
        if (input[[factortype_n]] == "numeric") {
          finalstring = c(
            finalstring,
            "seq(",
            input[[numericlow_n]],
            ",",
            input[[numerichigh_n]],
            ", length.out = ",
            input[[numericlength_n]],
            ")"
          )
        }
        if (input[[factortype_n]] == "discnum") {
          finalstring = c(
            finalstring,
            "c(",
            gsub(" ", "", input[[disclevels_n]], fixed = TRUE),
            ")"
          )
        }
        if (input[[factortype_n]] == "cat") {
          levelstring = paste0(
            c("\""),
            strsplit(
              gsub(" ", "", input[[levels_n]], fixed = TRUE),
              split = ","
            )[[1]],
            c("\","),
            collapse = ""
          )
          levelstring = substr(levelstring, 1, nchar(levelstring) - 1)
          finalstring = c(finalstring, "c(", levelstring, ")")
        }
        if (commacount > 0) {
          commacount = commacount - 1
          finalstring = c(
            finalstring,
            paste0(c(", \n", rep("&nbsp;", 27)), collapse = "")
          )
        }
      }
      finalstring
    })

    regularmodelstring = reactive({
      tryCatch(
        {
          if (
            any(
              unlist(strsplit(
                as.character(as.formula(input$model)[2]),
                "\\s\\+\\s|\\s\\*\\s|\\:"
              )) ==
                "."
            )
          ) {
            dotreplace = paste0(
              "(",
              paste0(names(candidatesetall()), collapse = " + "),
              ")"
            )
            additionterms = unlist(strsplit(
              as.character(as.formula(input$model)[2]),
              "\\s\\+\\s"
            ))
            multiplyterms = unlist(lapply(
              lapply(
                strsplit(additionterms, split = "\\s\\*\\s"),
                gsub,
                pattern = "^\\.$",
                replacement = dotreplace
              ),
              paste0,
              collapse = " * "
            ))
            interactionterms = unlist(lapply(
              lapply(
                strsplit(multiplyterms, split = "\\:"),
                gsub,
                pattern = "^\\.$",
                replacement = dotreplace
              ),
              paste0,
              collapse = ":"
            ))
            stringmodel = paste0(
              "~",
              paste(interactionterms, collapse = " + "),
              sep = ""
            )
          }
          paste0(as.character(as.formula(stringmodel)), collapse = "")
        },
        error = function(e) {
          paste0(input$model, collapse = "")
        }
      )
    })

    modelwithblocks = reactive({
      if (isblockingtext()) {
        basemodel = gsub(
          pattern = "~",
          replacement = "",
          x = regularmodelstring(),
          fixed = TRUE
        )
        blockingmodelterms = "~ (1|Block1) + "
        paste0(blockingmodelterms, basemodel)
      }
    })

    contraststring = reactive({
      factor_cat = list()
      name_cat = list()

      for (i in seq_len(input$numberfactors)) {
        factorname_n = sprintf("factorname%i", i)
        factortype_n = sprintf("factortype%i", i)
        name_cat[[i]] = input[[factorname_n]]
        factor_cat[[i]] = input[[factortype_n]]
      }
      factor_cat = unlist(factor_cat)
      fac_idx = which(factor_cat == "cat")
      last_idx = tail(fac_idx, 1)
      namecat = unlist(name_cat)
      contrasttemp = "list("
      for (i in fac_idx) {
        if (i != last_idx) {
          contrasttemp = paste0(contrasttemp, namecat[i], " = ", "contr.sum, ")
        } else {
          contrasttemp = paste0(contrasttemp, namecat[i], " = ", "contr.sum)")
        }
      }
      contrasttemp
    })

    anyfactors = reactive({
      fac = FALSE
      for (i in seq_len(input$numberfactors)) {
        factortype_n = sprintf("factortype%i", i)
        if (input[[factortype_n]] == "cat") {
          fac = TRUE
        }
      }
      fac
    })

    code = reactive({
      req(inputstring(), cancelOutput = TRUE)
      blocking = any_htc()
      first = paste0(
        c(
          "<br><pre>",
          "<code style=\"color:#468449\"># This is the R code used to generate these results in skpr.</code><br>",
          "<code style=\"color:#468449\"># Copy this into an R script and rerun to reproduce these results.</code><br><br>",
          "library(skpr)<br><br>",
          ifelse(
            input$setseed,
            paste0(
              "<code style=\"color:#468449\">#Setting random number generator seed:</code><br>",
              "set.seed(",
              input$seed,
              ")<br><br>"
            ),
            "<code style=\"color:#468449\">#Consider setting a seed to make this script fully reproducible.<br>#Go to Advanced->Set Random Number Generator Seed, click <br>#the checkbox, and set Random Seed to any whole number.</code><br><br>"
          ),
          "<code style=\"color:#468449\"># Generating candidate set:</code><br>",
          "candidateset = expand.grid(",
          inputstring(),
          ")<br><br>",
          ifelse(
            blocking,
            paste0(
              c(
                "<code style=\"color:#468449\"># Generating design for hard-to-change factors:</code> <br>",
                "design_htc = gen_design(candidateset = candidateset, <br>",
                rep("&nbsp;", 24),
                "model = ",
                blockmodel(),
                ", <br>",
                rep("&nbsp;", 24),
                "trials = ",
                as.character(input$numberblocks),
                ")<br><br>"
              ),
              collapse = ""
            ),
            ""
          ),
          "<code style=\"color:#468449\"># Generating design:</code><br>",
          "design = gen_design(candidateset = candidateset, <br>",
          rep("&nbsp;", 20),
          "model = ",
          regularmodelstring(),
          ", <br>",
          rep("&nbsp;", 20),
          "trials = ",
          as.character(input$trials)
        ),
        collapse = ""
      )
      if (blocking) {
        first = paste(
          c(first, ", <br>", rep("&nbsp;", 20), "splitplotdesign = design_htc"),
          collapse = ""
        )
      }
      if (input$optimality != "D") {
        first = paste(
          c(
            first,
            ", <br>",
            rep("&nbsp;", 20),
            "optimality = \"",
            input$optimality,
            "\""
          ),
          collapse = ""
        )
      }
      if (input$repeats != 20) {
        first = paste(
          c(first, ", <br>", rep("&nbsp;", 20), "repeats = ", input$repeats),
          collapse = ""
        )
      }
      if (input$varianceratio != 1) {
        first = paste(
          c(
            first,
            ", <br>",
            rep("&nbsp;", 20),
            "varianceratio = ",
            input$varianceratio
          ),
          collapse = ""
        )
      }
      if (input$aliaspower != 2) {
        first = paste(
          c(
            first,
            ", <br>",
            rep("&nbsp;", 20),
            "aliaspower = ",
            input$aliaspower
          ),
          collapse = ""
        )
      }
      if (input$mindopt != 0.8) {
        first = paste(
          c(first, ", <br>", rep("&nbsp;", 20), "minDopt = ", input$mindopt),
          collapse = ""
        )
      }
      if (as.logical(input$parallel)) {
        first = paste(
          c(first, ", <br>", rep("&nbsp;", 20), "parallel = TRUE"),
          collapse = ""
        )
      }
      if (isblockingtext()) {
        first = paste(
          c(
            first,
            ", <br>",
            rep("&nbsp;", 20),
            "add_blocking_columns = ",
            ifelse(input$splitanalyzable, "TRUE", "FALSE")
          ),
          collapse = ""
        )
      }
      first = paste0(c(first, ")<br><br>"), collapse = "")
      if (input$evaltype == "lm") {
        first = paste0(
          c(
            first,
            "<code style=\"color:#468449\"># Evaluating Design:</code><br>",
            "eval_design(design = design, <br>",
            rep("&nbsp;", 12),
            "model = ",
            regularmodelstring(),
            ", <br>",
            rep("&nbsp;", 12),
            "alpha = ",
            input$alpha
          ),
          collapse = ""
        )
        if (isblockingtext()) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 12), "blocking = TRUE"),
            collapse = ""
          )
        }
        if (input$snr != 2) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 12), "effectsize = ", input$snr),
            collapse = ""
          )
        }
        if (input$varianceratio != 1) {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 12),
              "varianceratios = ",
              input$varianceratio
            ),
            collapse = ""
          )
        }
        if (as.logical(input$conservative)) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 12), "conservative = TRUE"),
            collapse = ""
          )
        }
        if (as.logical(input$detailedoutput)) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 12), "detailedoutput = TRUE"),
            collapse = ""
          )
        }
        first = paste0(c(first, ")<br><br>"), collapse = "")
        first = paste0(
          first,
          "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
          "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
          "<code style=\"color:#468449\">## First, assign the results to a column in the data frame. Each </code><br>",
          "<code style=\"color:#468449\">## entry in the vector corresponds to the result from that run in the design. <br><br></code>",
          "<code style=\"color:#468449\">#design$Y = results <br><br></code>",
          ifelse(
            !isblockingtext(),
            "<code style=\"color:#468449\">## Now analyze the linear model with lm:</code><br><br>",
            "<code style=\"color:#468449\">## Now analyze the blocked linear model with lmer (from the lme4 package) and lmerTest:<br><br></code>"
          ),
          ifelse(
            !isblockingtext(),
            paste0(
              "<code style=\"color:#468449\">#lm(formula = Y ",
              regularmodelstring(),
              ", data = design",
              ifelse(
                anyfactors(),
                paste0(
                  ", </code><br><code style=\"color:#468449\">#   ",
                  "contrasts = ",
                  contraststring(),
                  ")</code>"
                ),
                ")<br><br></code>"
              )
            ),
            paste0(
              ifelse(
                input$splitanalyzable,
                "",
                "<code style=\"color:#468449\">## Note: Argument add_blocking_columns needs to be active in last gen_design call in order<br>## to analyze data taking into account the split-plot structure. The code below assumes that is true. <br><br></code>"
              ),
              "<code style=\"color:#468449\">#library(lmerTest)<br>#lme4::lmer(formula = Y ",
              modelwithblocks(),
              ", data = design",
              ifelse(
                anyfactors(),
                paste0(
                  ", <br>#          ",
                  "contrasts = ",
                  contraststring(),
                  "))<br><br>"
                ),
                "))<br><br></code>"
              )
            )
          )
        )
      }
      if (input$evaltype == "glm") {
        first = paste0(
          c(
            first,
            "<code style=\"color:#468449\"># Evaluating (Monte Carlo) Design:</code><br>",
            "eval_design_mc(design = design, <br>",
            rep("&nbsp;", 15),
            "model = ",
            regularmodelstring(),
            ", <br>",
            rep("&nbsp;", 15),
            "alpha = ",
            input$alpha
          ),
          collapse = ""
        )
        if (isblockingtext()) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 15), "blocking = TRUE"),
            collapse = ""
          )
        }
        if (input$nsim != 1000) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 15), "nsim = ", input$nsim),
            collapse = ""
          )
        }
        if (input$glmfamily != "gaussian") {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 15),
              "glmfamily = \"",
              input$glmfamily,
              "\""
            ),
            collapse = ""
          )
        }
        if (input$adjust_alpha) {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 15),
              "adjust_alpha_inflation  = TRUE"
            ),
            collapse = ""
          )
        } else {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 15),
              "adjust_alpha_inflation = FALSE"
            ),
            collapse = ""
          )
        }
        if (length(effectsize()) == 1) {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 15),
              "effectsize = ",
              effectsize()
            ),
            collapse = ""
          )
        } else {
          effectsize2 = effectsize()
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 15),
              "effectsize = c(",
              effectsize2[1],
              ", ",
              effectsize2[2],
              ")"
            ),
            collapse = ""
          )
        }
        if (!is.null(input$varianceratios)) {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 15),
              "varianceratios = ",
              input$varianceratio
            ),
            collapse = ""
          )
        }
        if (as.logical(input$parallel_eval_glm)) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 15), "parallel = TRUE"),
            collapse = ""
          )
        }
        if (as.logical(input$detailedoutput)) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 15), "detailedoutput = TRUE"),
            collapse = ""
          )
        }
        first = paste0(c(first, ")<br><br>"), collapse = "")
        first = paste0(
          first,
          "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
          "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
          "<code style=\"color:#468449\">## First, assign the results to a column in the data frame. Each </code><br>",
          "<code style=\"color:#468449\">## entry in the vector corresponds to the result from that run in the design. <br><br></code>",
          "<code style=\"color:#468449\">#design$Y = results <br><br></code>",
          ifelse(
            !isblockingtext(),
            "<code style=\"color:#468449\">## Now analyze the generalized linear model with glm:</code><br><br>",
            "<code style=\"color:#468449\">## Now analyze the blocked generalized linear model with glmer (from the lme4 package):<br><br></code>"
          ),
          ifelse(
            !isblockingtext(),
            paste0(
              "<code style=\"color:#468449\">#glm(formula = Y ",
              regularmodelstring(),
              ", data = design",
              ", <br>#   family = ",
              ifelse(
                input$glmfamily == "exponential",
                "Gamma(link=\"log\")",
                paste0("\"", input$glmfamily, "\"")
              ),
              ifelse(
                anyfactors(),
                paste0(
                  ", </code><br><code style=\"color:#468449\">#   ",
                  "contrasts = ",
                  contraststring(),
                  ")</code>"
                ),
                ")<br><br></code>"
              )
            ),
            paste0(
              ifelse(
                input$splitanalyzable,
                "",
                "<code style=\"color:#468449\">## Note: Argument add_blocking_columns needs to be active in last gen_design call in order<br>## to analyze data taking into account the split-plot structure. The code below assumes that is true. <br><br></code>"
              ),
              "<code style=\"color:#468449\">#lme4::glmer(formula = Y ",
              modelwithblocks(),
              ", data = design",
              ", <br>#          family = ",
              ifelse(
                input$glmfamily == "exponential",
                "Gamma(link=\"log\")",
                paste0("\"", input$glmfamily, "\"")
              ),
              ifelse(
                anyfactors(),
                paste0(
                  ", <br>#          ",
                  "contrasts = ",
                  contraststring(),
                  ")"
                ),
                ")</code>"
              )
            )
          )
        )
      }
      if (input$evaltype == "surv") {
        first = paste0(
          c(
            first,
            "<code style=\"color:#468449\"># Evaluating (Monte Carlo Survival) Design:</code><br>",
            "eval_design_survival_mc(design = design, <br>",
            rep("&nbsp;", 24),
            "model = ",
            as.character(as.formula(input$model)),
            ", <br>",
            rep("&nbsp;", 24),
            "alpha = ",
            input$alpha
          ),
          collapse = ""
        )
        if (input$nsim_surv != 1000) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 24), "nsim = ", input$nsim_surv),
            collapse = ""
          )
        }
        if (input$distribution != "gaussian") {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 24),
              "distribution = \"",
              input$distribution,
              "\""
            ),
            collapse = ""
          )
        }
        if (length(effectsize()) == 1) {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 24),
              "effectsize = ",
              effectsize()
            ),
            collapse = ""
          )
        } else {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 24),
              "effectsize = c(",
              effectsize()[1],
              ", ",
              effectsize()[2],
              ")"
            ),
            collapse = ""
          )
        }
        if (!is.na(input$censorpoint)) {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 24),
              "censorpoint = ",
              input$censorpoint
            ),
            collapse = ""
          )
        }
        if (input$censortype != "right") {
          first = paste(
            c(
              first,
              ", <br>",
              rep("&nbsp;", 24),
              "censortype = \"",
              input$censortype,
              "\""
            ),
            collapse = ""
          )
        }
        if (as.logical(input$parallel_eval_surv)) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 24), "parallel = TRUE"),
            collapse = ""
          )
        }
        if (as.logical(input$detailedoutput)) {
          first = paste(
            c(first, ", <br>", rep("&nbsp;", 24), "detailedoutput = TRUE"),
            collapse = ""
          )
        }
        first = paste0(c(first, ")<br><br>"), collapse = "")
        first = paste0(
          first,
          "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
          "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
          "<code style=\"color:#468449\">## This is a survival model, so first create a Surv object that marks the censored runs.</code><br>",
          "<code style=\"color:#468449\">## Each entry in the results vector corresponds to the result from that run in the design.</code><br>",
          "<code style=\"color:#468449\">## Here, the raw censored responses are given as NA. We replace those values with the</code><br>",
          "<code style=\"color:#468449\">## censor point and use the event argument to mark them as censored. </code><br>",
          "<code style=\"color:#468449\">## (Surv argument \"event\" is TRUE when \"results\" is not censored, and FALSE when censored).<br><br></code>",
          "<code style=\"color:#468449\">#notcensored = !is.na(results) </code><br>",
          ifelse(
            !is.na(input$censorpoint),
            paste0(
              "<code style=\"color:#468449\">#results[is.na(results)] = ",
              input$censorpoint,
              "</code><br>"
            ),
            ""
          ),
          "<code style=\"color:#468449\">#design$Y = Surv(time=results, event = notcensored, type = \"",
          input$censortype,
          "\") <br><br></code>",
          "<code style=\"color:#468449\">## Now analyze the linear model with survreg (from the survival package):</code><br><br>",
          paste0(
            "<code style=\"color:#468449\">#survival::survreg(formula = Y ",
            regularmodelstring(),
            ", data = design, dist = \"",
            input$distribution,
            "\")"
          )
        )
      }

      first = paste0(c(first, "</pre></code>"), collapse = "")
      first
    })

    isblocking = reactive({
      any_htc()
    }) |>
      bindEvent(input$submitbutton)

    isblockingtext = reactive({
      any_htc()
    })

    blockmodel = reactive({
      if (isblocking()) {
        if (input$model == "~.") {
          as.formula(paste0(
            "~",
            paste(names(inputlist_htctext()), collapse = " + ")
          ))
        } else {
          names = names(inputlist())
          modelsplit = attr(
            terms.formula(as.formula(input$model), data = candidatesetall()),
            "term.labels"
          )
          regularmodel = rep(FALSE, length(modelsplit))
          for (term in names) {
            regex = paste0(
              "(\\b",
              term,
              "\\b)|(\\b",
              term,
              ":)|(:",
              term,
              "\\b)|(\\b",
              term,
              "\\s\\*)|(\\*\\s",
              term,
              "\\b)|(:",
              term,
              ":)"
            )
            regularmodel = regularmodel | grepl(regex, modelsplit, perl = TRUE)
          }
          as.formula(paste0(
            "~",
            paste(modelsplit[!regularmodel], collapse = " + ")
          ))
        }
      }
    })

    optimality = reactive({
      input$submitbutton
      if (
        isolate(input$numberfactors) == 1 &&
          isolate(input$optimality) == "Alias"
      ) {
        showNotification(
          "Alias-optimal design selected with only one factor: Switching to D-optimal.",
          type = "warning",
          duration = 10
        )
        updateSelectInput(
          session,
          "optimality",
          choices = c("D", "I", "A", "Alias", "G", "E", "T"),
          selected = "D"
        )
        "D"
      } else {
        isolate(input$optimality)
      }
    })

    effectsize = reactive({
      if (input$evaltype == "lm") {
        return(input$snr)
      }
      if (input$evaltype == "glm") {
        if (input$glmfamily == "gaussian") {
          return(input$snr)
        }
        if (input$glmfamily == "binomial") {
          return(c(input$binomialprobs[1], input$binomialprobs[2]))
        }
        if (input$glmfamily == "poisson") {
          return((c(input$poislow, input$poishigh)))
        }
        if (input$glmfamily == "exponential") {
          return(c(input$explow, input$exphigh))
        }
      }
      if (input$evaltype == "surv") {
        if (input$distribution == "gaussian") {
          return(input$snr)
        }
        if (input$distribution == "lognormal") {
          return(input$snr)
        }
        if (input$distribution == "exponential") {
          return(c(input$explow, input$exphigh))
        }
      }
    })

    ###### Run Matrix ######
    runmatrix_container = reactive(
      {
        if (!multiuser) {
          on.exit(
            {
              shinyjs::enable("evalbutton")
              shinyjs::enable("submitbutton")
              current_design_valid(TRUE)
            },
            add = TRUE
          )
          shinyjs::disable("submitbutton")
          shinyjs::disable("evalbutton")
          if (input$setseed) {
            set.seed(input$seed)
          }
          if (!as.logical(input$parallel) && skpr_progress) {
            pb = inc_progress_session
          } else {
            pb = NULL
          }
          progress_wrapper = function(code) {
            if (skpr_progress) {
              if (!as.logical(input$parallel)) {
                withProgress(
                  message = "Generating design:",
                  value = 0,
                  min = 0,
                  max = 1,
                  expr = code
                )
              } else {
                if (!isblocking()) {
                  max_val = 1
                } else {
                  max_val = 2
                }
                progressr::withProgressShiny(
                  message = "Generating design:",
                  value = 0,
                  min = 0,
                  max = max_val,
                  expr = code,
                  handlers = c(shiny = progressr::handler_shiny)
                )
              }
            } else {
              code
            }
          }
          candidateset = expand.grid(candidatesetall())
          model = as.formula(input$model)
          trials = input$trials
          repeats = input$repeats
          optimality = optimality()
          parallel = as.logical(input$parallel)
          minDopt = input$mindopt
          aliaspower = input$aliaspower
          advancedoptions = list(GUI = TRUE, progressBarUpdater = pb)
          if (!isblocking()) {
            progress_wrapper({
              gen_design(
                candidateset = candidateset,
                model = model,
                trials = trials,
                optimality = optimality,
                repeats = repeats,
                aliaspower = aliaspower,
                minDopt = minDopt,
                parallel = parallel,
                advancedoptions = advancedoptions
              )
            })
          } else {
            blockmodel = blockmodel()
            numberblocks = input$numberblocks
            block_optimality = ifelse(
              toupper(optimality()) == "ALIAS" &&
                length(inputlist_htc()) == 1,
              "D",
              optimality()
            )
            varianceratio = input$varianceratio
            add_blocking_columns = input$splitanalyzable
            progress_wrapper({
              spd = gen_design(
                candidateset = candidateset,
                model = blockmodel,
                trials = numberblocks,
                optimality = block_optimality,
                repeats = repeats,
                varianceratio = varianceratio,
                aliaspower = aliaspower,
                minDopt = minDopt,
                parallel = parallel,
                advancedoptions = advancedoptions
              )
              gen_design(
                candidateset = candidateset,
                model = model,
                trials = trials,
                splitplotdesign = spd,
                optimality = optimality,
                repeats = repeats,
                varianceratio = varianceratio,
                aliaspower = aliaspower,
                minDopt = minDopt,
                parallel = parallel,
                add_blocking_columns = add_blocking_columns,
                advancedoptions = advancedoptions
              )
            })
          }
        } else {
          shinyjs::disable("submitbutton")
          shinyjs::disable("evalbutton")
          prog_env$previouspercent = 0
          prog_env$percentdone = 0
          prog_env$need_to_initialize_progress_bar = TRUE
          prog_env$progress = Progress$new()
          prog_env$progress_type = "gen"
          prog_env$progress$set(
            message = "Setting up design search...",
            value = 0
          )
          if (input$setseed) {
            seed_val = input$seed
          } else {
            seed_val = NULL
          }
          candidateset = expand.grid(candidatesetall())
          model = as.formula(input$model)
          trials = input$trials
          repeats = input$repeats
          optimality = optimality()
          parallel = as.logical(input$parallel)
          minDopt = input$mindopt
          aliaspower = input$aliaspower
          advancedoptions = list(
            GUI = TRUE,
            progressBarUpdater = multiuser_progress_bar_updater
          )

          if (!isblocking()) {
            future::future(
              {
                temp_design = gen_design(
                  candidateset = candidateset,
                  model = model,
                  trials = trials,
                  optimality = optimality,
                  repeats = repeats,
                  aliaspower = aliaspower,
                  minDopt = minDopt,
                  parallel = parallel,
                  advancedoptions = advancedoptions
                )
                attr(temp_design, "generating.model") = NULL
                saveRDS(temp_design, file = file.path(runmatrix_file_name))
              },
              seed = seed_val
            )
          } else {
            blockmodel = blockmodel()
            numberblocks = input$numberblocks
            block_optimality = ifelse(
              toupper(optimality()) == "ALIAS" &&
                length(inputlist_htc()) == 1,
              "D",
              optimality()
            )
            varianceratio = input$varianceratio
            add_blocking_columns = input$splitanalyzable
            future::future(
              {
                spd = gen_design(
                  candidateset = candidateset,
                  model = blockmodel,
                  trials = numberblocks,
                  optimality = block_optimality,
                  repeats = repeats,
                  varianceratio = varianceratio,
                  aliaspower = aliaspower,
                  minDopt = minDopt,
                  parallel = parallel,
                  advancedoptions = advancedoptions
                )
                temp_design = gen_design(
                  candidateset = candidateset,
                  model = model,
                  trials = trials,
                  splitplotdesign = spd,
                  optimality = optimality,
                  repeats = repeats,
                  varianceratio = varianceratio,
                  aliaspower = aliaspower,
                  minDopt = minDopt,
                  parallel = parallel,
                  add_blocking_columns = add_blocking_columns,
                  advancedoptions = advancedoptions
                )
                attr(temp_design, "generating.model") = NULL
                saveRDS(temp_design, file = file.path(runmatrix_file_name))
              },
              seed = seed_val
            )
          }
        }
      },
      label = "runmatrix_container"
    ) |>
      bindEvent(input$submitbutton)

    evaluationtype = reactive({
      input$evaltype
    }) |>
      bindEvent(input$evalbutton)

    format_table = function(powerval, display_table, alpha, nsim, colorblind) {
      color_bad = "red"
      color_maybe = "yellow"
      if (colorblind) {
        color_bad = "purple"
        color_maybe = "orange"
      }
      desired_power_level = input$desired_power
      get_lcbs = function(power) {
        if (evaluationtype() == "lm") {
          return(power)
        } else {
          return(unlist(lapply(
            power,
            \(x) (binom.test(x * nsim, n = nsim)[["conf.int"]][1])
          )))
        }
      }
      get_ucbs = function(power) {
        if (evaluationtype() == "lm") {
          return(power)
        } else {
          return(unlist(lapply(
            power,
            \(x) (binom.test(x * nsim, n = nsim)[["conf.int"]][2])
          )))
        }
      }
      display_table = display_table %>%
        gt::data_color(
          columns = "power",
          palette = scales::col_numeric(
            palette = colorRampPalette(c("white", "darkgreen"))(100),
            domain = c(0, 1)
          ),
          domain = c(0, 1),
          alpha = 0.3,
          autocolor_text = FALSE
        ) %>%
        gt::tab_options(table.width = gt::pct(100))
      if (evaluationtype() != "lm") {
        nsim_digits = floor(log10(nsim))
        format_string = sprintf("%%0.%if-%%0.%if", nsim_digits, nsim_digits)
        display_table = display_table %>%
          gt::cols_add(
            Error = sprintf(
              format_string,
              get_lcbs(powerval$power),
              get_ucbs(powerval$power)
            )
          )
      }
      if (
        any(
          get_lcbs(powerval$power) <= desired_power_level &
            get_ucbs(powerval$power) >= desired_power_level
        )
      ) {
        display_table = display_table %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = color_maybe, alpha = 0.3)
            ),
            locations = gt::cells_body(
              columns = "power",
              rows = get_lcbs(powerval$power) <= desired_power_level
            )
          ) %>%
          gt::tab_source_note(
            source_note = sprintf(
              "Note: Power values marked in %s are within the simulation uncertainty for user-specified Type-I error (increase the number of simulations)",
              color_maybe
            )
          )
      }
      if (any(get_ucbs(powerval$power) < desired_power_level)) {
        display_table = display_table %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = color_bad, alpha = 0.3)
            ),
            locations = gt::cells_body(
              columns = "power",
              rows = get_ucbs(powerval$power) < desired_power_level
            )
          ) %>%
          gt::tab_source_note(
            source_note = sprintf(
              "Note: Power values marked in %s fall below the user-specified Type-I error (%0.2f)",
              color_bad,
              alpha
            )
          )
      }
      return(display_table)
    }

    observeEvent(input$evalbutton, {
      if (!current_design_valid()) {
        showNotification(
          "Warning: Design generation inputs have potentially changed but new design has not been generated. Evaluation is for displayed design.",
          type = "warning"
        )
      }
    })

    ###### Power Results ######
    powerresults = reactive(
      {
        if (evaluationtype() == "lm") {
          runmatrix() %>%
            eval_design(
              model = as.formula(input$model),
              alpha = input$alpha,
              blocking = isblocking(),
              effectsize = effectsize(),
              conservative = input$conservative,
              detailedoutput = input$detailedoutput
            )
        }
      },
      label = "powerresults"
    ) |>
      bindEvent(input$evalbutton)

    ###### Power Results GLM ######
    powerresultsglm_container = reactive(
      {
        req(runmatrix(), cancelOutput = TRUE)
        if (evaluationtype() == "glm") {
          if (!multiuser) {
            if (!as.logical(input$parallel_eval_glm) && skpr_progress) {
              pb = inc_progress_session
            } else {
              pb = NULL
            }
            progress_wrapper = function(code) {
              if (input$adjust_alpha) {
                max_val = 2
              } else {
                max_val = 1
              }
              if (!input$adjust_alpha) {
                if (isblocking()) {
                  mess = "Evaluating power (with REML):"
                } else {
                  mess = "Evaluating power:"
                }
              } else {
                if (isblocking()) {
                  mess = "Evaluating power (with adjusted Type-I error and REML):"
                } else {
                  mess = "Evaluating power (with adjusted Type-I error):"
                }
              }
              if (skpr_progress) {
                if (!as.logical(input$parallel_eval_glm)) {
                  withProgress(
                    message = mess,
                    value = 0,
                    min = 0,
                    max = max_val,
                    expr = code
                  )
                } else {
                  progressr::withProgressShiny(
                    message = mess,
                    value = 0,
                    min = 0,
                    max = max_val,
                    expr = code,
                    handlers = c(shiny = progressr::handler_shiny)
                  )
                }
              } else {
                code
              }
            }
            if (isblocking() && input$firth_correction) {
              showNotification(
                "Firth correction not supported for blocked designs. Using un-penalized logistic regression.",
                type = "warning",
                duration = 10
              )
              firth_cor = FALSE
            } else {
              firth_cor = input$firth_correction
            }
            if (input$setseed) {
              set.seed(input$seed)
            }
            model = as.formula(input$model)
            alpha = input$alpha
            blocking = isblocking()
            nsim = input$nsim
            varianceratios = input$varianceratio
            glmfamily = input$glmfamily
            effectsize_val = effectsize()
            firth = firth_cor
            parallel = input$parallel_eval_glm
            adjust_alpha_inflation = input$adjust_alpha
            detailedoutput = input$detailedoutput
            advancedoptions = list(GUI = TRUE, progressBarUpdater = pb)
            progress_wrapper({
              eval_design_mc(
                runmatrix(),
                model = model,
                alpha = alpha,
                blocking = blocking,
                nsim = nsim,
                varianceratios = varianceratios,
                glmfamily = glmfamily,
                effectsize = effectsize_val,
                firth = firth,
                parallel = parallel,
                adjust_alpha_inflation = adjust_alpha_inflation,
                detailedoutput = detailedoutput,
                advancedoptions = advancedoptions
              )
            })
          } else {
            prog_env$previouspercent = 0
            prog_env$percentdone = 0
            prog_env$need_to_initialize_progress_bar = TRUE
            prog_env$progress = Progress$new()
            prog_env$progress_type = "glm"
            prog_env$progress$set(
              message = "Setting up simulation...",
              value = 0
            )
            if (isblocking() && input$firth_correction) {
              showNotification(
                "Firth correction not supported for blocked designs. Using un-penalized logistic regression.",
                type = "warning",
                duration = 10
              )
              firth_cor = FALSE
            } else {
              firth_cor = input$firth_correction
            }
            if (input$setseed) {
              set.seed(input$seed)
            }
            model = as.formula(input$model)
            alpha = input$alpha
            blocking = isblocking()
            nsim = input$nsim
            varianceratios = input$varianceratio
            glmfamily = input$glmfamily
            effectsize_val = effectsize()
            firth = firth_cor
            parallel = input$parallel_eval_glm
            adjust_alpha_inflation = input$adjust_alpha
            detailedoutput = input$detailedoutput
            advancedoptions = list(
              GUI = TRUE,
              progressBarUpdater = multiuser_progress_bar_updater
            )

            if (input$setseed) {
              seed_val = input$seed
            } else {
              seed_val = NULL
            }
            runmat = runmatrix()
            shinyjs::disable("submitbutton")
            shinyjs::disable("evalbutton")
            future::future(
              {
                temp_power = eval_design_mc(
                  runmat,
                  model = model,
                  alpha = alpha,
                  blocking = blocking,
                  nsim = nsim,
                  varianceratios = varianceratios,
                  glmfamily = glmfamily,
                  effectsize = effectsize_val,
                  firth = firth,
                  parallel = parallel,
                  adjust_alpha_inflation = adjust_alpha_inflation,
                  detailedoutput = detailedoutput,
                  advancedoptions = advancedoptions
                )
                attr(temp_power, "generating.model") = NULL
                saveRDS(temp_power, file = file.path(power_file_name))
              },
              seed = seed_val
            )
          }
        }
      },
      label = "powerresultsglm_container"
    ) |>
      bindEvent(input$evalbutton)

    ###### Power Results Survival ######
    powerresultssurv_container = reactive({
      req(runmatrix(), cancelOutput = TRUE)

      if (
        !multiuser && (!as.logical(input$parallel_eval_surv) && skpr_progress)
      ) {
        pb = inc_progress_session
      } else {
        pb = NULL
      }
      progress_wrapper = function(code) {
        mess = "Evaluating design:"
        if (skpr_progress) {
          if (!as.logical(input$parallel_eval_surv)) {
            withProgress(
              message = mess,
              value = 0,
              min = 0,
              max = 1,
              expr = code
            )
          } else {
            progressr::withProgressShiny(
              message = mess,
              value = 0,
              min = 0,
              max = 1,
              expr = code,
              handlers = c(shiny = progressr::handler_shiny)
            )
          }
        } else {
          code
        }
      }
      if (evaluationtype() == "surv") {
        if (input$distribution == "lognormal" && input$censorpoint <= 0) {
          showNotification(
            sprintf(
              "When calculating power for a lognormal survival model, the censor point must be greater than 0 (currently set to %0.2f).",
              input$censorpoint
            ),
            type = "warning",
            duration = 10
          )
          req(
            input$distribution == "lognormal" && input$censorpoint > 0,
            cancelOutput = TRUE
          )
        } else {
          if (input$setseed) {
            set.seed(input$seed)
          }
          if (isblocking()) {
            showNotification(
              "Hard-to-change factors are not supported for survival designs. Evaluating design with no blocking.",
              type = "Warning",
              duration = 10
            )
          }
          model = as.formula(input$model)
          alpha = input$alpha
          nsim = input$nsim_surv
          censorpoint = input$censorpoint
          censortype = input$censortype
          distribution = input$distribution
          parallel = input$parallel_eval_surv
          effectsize_val = effectsize()
          detailedoutput = input$detailedoutput
          advancedoptions = list(GUI = TRUE, progressBarUpdater = pb)
          runmat = runmatrix()
          if (!multiuser) {
            progress_wrapper({
              eval_design_survival_mc(
                runmatrix(),
                model = model,
                alpha = alpha,
                nsim = nsim,
                censorpoint = censorpoint,
                censortype = censortype,
                distribution = distribution,
                parallel = parallel,
                effectsize = effectsize_val,
                detailedoutput = detailedoutput,
                advancedoptions = advancedoptions
              )
            })
          } else {
            prog_env$previouspercent = 0
            prog_env$percentdone = 0
            prog_env$need_to_initialize_progress_bar = TRUE
            prog_env$progress = Progress$new()
            prog_env$progress_type = "surv"
            prog_env$progress$set(
              message = "Setting up simulation...",
              value = 0
            )
            if (input$setseed) {
              seed_val = input$seed
            } else {
              seed_val = NULL
            }
            advancedoptions = list(
              GUI = TRUE,
              progressBarUpdater = multiuser_progress_bar_updater
            )
            shinyjs::disable("submitbutton")
            shinyjs::disable("evalbutton")
            run_matrix = runmatrix()

            future::future(
              {
                temp_power = eval_design_survival_mc(
                  run_matrix,
                  model = model,
                  alpha = alpha,
                  nsim = nsim,
                  censorpoint = censorpoint,
                  censortype = censortype,
                  distribution = distribution,
                  parallel = parallel,
                  effectsize = effectsize_val,
                  detailedoutput = detailedoutput,
                  advancedoptions = advancedoptions
                )
                attr(temp_power, "generating.model") = NULL
                saveRDS(temp_power, file = file.path(power_file_name))
              },
              seed = seed_val
            )
          }
        }
      }
    }) |>
      bindEvent(input$evalbutton)

    pal_option = function() {
      if (input$colorchoice == "A") {
        viridis::magma
      } else if (input$colorchoice == "B") {
        viridis::inferno
      } else if (input$colorchoice == "C") {
        viridis::plasma
      } else if (input$colorchoice == "D") {
        viridis::viridis
      } else {
        function(x) return("white")
      }
    }

    style_matrix = function(
      runmat,
      order_vals = FALSE,
      alpha = 0.3,
      trials,
      optimality,
      pal_choice
    ) {
      . = NULL
      if (order_vals) {
        reorder_runmat = function(run_mat) {
          new_runmat = runmat[do.call(order, runmat), , drop = FALSE]
          rownames(new_runmat) = 1:nrow(new_runmat)
          return(new_runmat)
        }

        display_rm = runmat %>%
          reorder_runmat() %>%
          gt::gt(rownames_to_stub = TRUE) %>%
          gt::tab_stubhead("Run") %>%
          gt::tab_options(data_row.padding = gt::px(10)) %>%
          gt::tab_spanner(
            label = "Factors",
            columns = colnames(.)
          ) %>%
          gt::tab_header(
            title = "Design",
            subtitle = sprintf("%d-run %s-optimal design", trials, optimality)
          )
      } else {
        display_rm = runmat %>%
          gt::gt(rownames_to_stub = TRUE) %>%
          gt::tab_stubhead("Run") %>%
          gt::tab_options(data_row.padding = gt::px(10)) %>%
          gt::tab_spanner(
            label = "Factors",
            columns = colnames(.)
          ) %>%
          gt::tab_header(
            title = "Design",
            subtitle = sprintf("%d-run %s-optimal design", trials, optimality)
          )
      }
      cols_rm = colnames(runmat)
      for (i in seq_len(length(cols_rm))) {
        if (is.numeric(runmat[, i])) {
          display_rm = display_rm %>%
            gt::data_color(
              columns = cols_rm[i],
              palette = pal_choice(100),
              alpha = alpha,
              autocolor_text = FALSE
            )
        } else {
          display_rm = display_rm %>%
            gt::data_color(
              columns = cols_rm[i],
              palette = pal_choice(length(unique(runmat[, i]))),
              alpha = alpha,
              autocolor_text = FALSE
            )
        }
      }
      display_rm
    }

    output$runmatrix = gt::render_gt(
      {
        req(runmatrix(), cancelOutput = TRUE)
        ord_design = input$orderdesign
        trials = isolate(input$trials)
        opt = isolate(input$optimality)
        pal_choice = pal_option()
        runmatrix() %>%
          style_matrix(
            order_vals = ord_design,
            trials = trials,
            optimality = opt,
            pal_choice = pal_choice
          )
      },
      align = "left"
    )

    filter_power_results = function(results) {
      col_results = colnames(results)
      results[,
        !col_results %in%
          c("glmfamily", "trials", "nsim", "blocking", "power_ucb", "power_lcb")
      ]
    }

    output$powerresults = gt::render_gt(
      {
        req(powerresults(), cancelOutput = TRUE)
        alpha = isolate(input$alpha)
        colorblind = isolate(input$colorblind)
        pwr_results = filter_power_results(powerresults())
        format_table(pwr_results, gt::gt(pwr_results), alpha, 0, colorblind)
      },
      align = "left"
    )

    output$powerresultsglm = gt::render_gt(
      {
        req(powerresultsglm(), cancelOutput = TRUE)
        alpha = isolate(input$alpha)
        nsim = isolate(input$nsim)
        colorblind = isolate(input$colorblind)
        pwr_results = filter_power_results(powerresultsglm())
        format_table(pwr_results, gt::gt(pwr_results), alpha, nsim, colorblind)
      },
      align = "left"
    ) |>
      bindEvent(powerresultsglm())

    output$powerresultssurv = gt::render_gt(
      {
        req(powerresultssurv(), cancelOutput = TRUE)
        alpha = isolate(input$alpha)
        nsim_surv = isolate(input$nsim_surv)
        colorblind = isolate(input$colorblind)
        pwr_results = filter_power_results(powerresultssurv())
        format_table(
          pwr_results,
          gt::gt(pwr_results),
          alpha,
          nsim_surv,
          colorblind
        )
      },
      align = "left"
    ) |>
      bindEvent(powerresultssurv())

    output$aliasplot = renderPlot({
      req(runmatrix(), cancelOutput = TRUE)
      if (displayed_design_number_factors() > 1) {
        runmatrix() %>%
          plot_correlations()
      }
    }) |>
      bindEvent(runmatrix(), ignoreInit = TRUE)

    output$fdsplot = renderPlot(
      {
        runmatrix() %>%
          plot_fds()
      },
      width = fdsplot_width
    ) |>
      bindEvent(input$submitbutton)

    output$code = renderUI({
      req(valid_code_pane(), cancelOutput = TRUE)
      HTML(code())
    })

    output$dopt = renderText({
      runmatrix() %>%
        attr("D")
    }) |>
      bindEvent(runmatrix())

    output$aopt = renderText({
      runmatrix() %>%
        attr("A")
    }) |>
      bindEvent(runmatrix())

    output$iopt = renderText({
      runmatrix() %>%
        attr("I")
    }) |>
      bindEvent(runmatrix())

    output$eopt = renderText({
      runmatrix() %>%
        attr("E")
    }) |>
      bindEvent(runmatrix())

    output$gopt = renderText({
      runmatrix() %>%
        attr("G")
    }) |>
      bindEvent(runmatrix())

    output$topt = renderText({
      runmatrix() %>%
        attr("T")
    }) |>
      bindEvent(runmatrix())

    output$optimalsearch = renderPlot(
      {
        req(runmatrix(), cancelOutput = TRUE)
        optimal_design_plot = function(runmat) {
          if (isolate(optimality()) %in% c("D", "G", "A")) {
            if (attr(runmat, "blocking") || attr(runmat, "splitplot")) {
              max_y_val = max(attr(runmat, "optimalsearchvalues"), na.rm = TRUE)
              statement = "Optimality Value (higher is better)"
            } else {
              max_y_val = 100
              statement = "Efficiency (higher is better)"
            }
            isolate(plot(
              attr(runmat, "optimalsearchvalues"),
              xlab = "Search Iteration",
              ylab = paste(optimality(), statement),
              type = "p",
              col = "red",
              pch = 16,
              ylim = c(0, max_y_val)
            ))
            isolate(points(
              x = attr(runmat, "best"),
              y = attr(runmat, "optimalsearchvalues")[attr(runmat, "best")],
              type = "p",
              col = "green",
              pch = 16,
              cex = 2,
              ylim = c(0, max_y_val)
            ))
          } else {
            if (isolate(optimality()) == "I") {
              isolate(plot(
                attr(runmat, "optimalsearchvalues"),
                xlab = "Search Iteration",
                ylab = "Average Prediction Variance (lower is better)",
                type = "p",
                col = "red",
                pch = 16
              ))
            } else {
              isolate(plot(
                attr(runmat, "optimalsearchvalues"),
                xlab = "Search Iteration",
                ylab = paste(optimality(), "Criteria Value (higher is better)"),
                type = "p",
                col = "red",
                pch = 16
              ))
            }
            isolate(points(
              x = attr(runmat, "best"),
              y = attr(runmat, "optimalsearchvalues")[attr(runmat, "best")],
              type = "p",
              col = "green",
              pch = 16,
              cex = 2
            ))
          }
        }
        runmatrix() %>%
          optimal_design_plot()
      },
      width = optimalsearch_plot_width
    ) |>
      bindEvent(runmatrix())

    output$simulatedpvalues = renderPlot(
      {
        plot_pvalue_histogram = function(poweresults) {
          pvalrows = isolate(floor(ncol(attr(poweresults, "pvals")) / 3) + 1)
          if (!is.null(attr(poweresults, "pvals"))) {
            par(mfrow = c(pvalrows, 3))
            for (col in 1:isolate(ncol(attr(poweresults, "pvals")))) {
              isolate(hist(
                attr(poweresults, "pvals")[, col],
                breaks = seq(0, 1, 0.05),
                main = colnames(attr(poweresults, "pvals"))[col],
                xlim = c(0, 1),
                xlab = "p values",
                ylab = "Count",
                col = "red",
                pch = 16
              ))
            }
          }
        }
        if (evaluationtype() == "glm") {
          powerresultsglm() %>%
            plot_pvalue_histogram()
        }
        if (evaluationtype() == "surv") {
          powerresultssurv() %>%
            plot_pvalue_histogram()
        }
      },
      width = pvalue_plot_width
    ) |>
      bindEvent(powerresultsglm(), powerresultssurv())

    output$parameterestimates = renderPlot(
      {
        plot_parameter_estimates = function(powerresults) {
          x = lcb = ucb = vals = type = ylab = NULL
          ylab = ifelse(
            isolate(input$glmfamily) == "binomial",
            "Parameter Estimates (Probability)",
            "Parameter Estimates"
          )
          if (!is.null(attr(powerresults, "estimates"))) {
            ests = apply(
              attr(powerresults, "estimates"),
              2,
              quantile,
              c(0.05, 0.5, 0.95),
              na.rm = TRUE
            )
            truth = attr(powerresults, "anticoef")
            if (isolate(input$glmfamily) == "binomial") {
              ests = exp(ests) / (1 + exp(ests))
              truth = exp(truth) / (1 + exp(truth))
            }
            if (isolate(input$glmfamily) == "poisson") {
              ests = exp(ests)
              truth = exp(truth)
            }
            if (isolate(input$glmfamily) == "exponential") {
              ests = exp(ests)
              truth = exp(truth)
            }
            ylim = ifelse(
              rep(isolate(input$glmfamily) == "binomial", 2),
              c(0, 1),
              c(min(as.vector(ests)), max(as.vector(ests)))
            )
            truth_df = data.frame(
              vals = c(truth, ests[2, ]),
              x = colnames(ests),
              type = c(
                rep("Truth", length(truth)),
                rep("Estimate", length(ests[2, ]))
              )
            )
            ests_ci_df = data.frame(
              lcb = ests[1, ],
              ucb = ests[3, ],
              x = colnames(ests)
            )

            ggplot() +
              geom_errorbar(
                data = ests_ci_df,
                aes(x = x, ymin = lcb, ymax = ucb),
                color = "black",
                width = 0.2
              ) +
              geom_point(data = truth_df, aes(x = x, y = vals, color = type)) +
              scale_color_manual("Type", values = c("red", "blue")) +
              scale_y_continuous(ylab, limits = ylim) +
              scale_x_discrete("Parameters", labels = colnames(ests)) +
              theme_light() +
              theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                text = element_text(size = 16)
              ) +
              labs(
                title = "Simulated Parameter Estimates (5%-95% Confidence Intervals)"
              )
          }
        }
        powerresultsglm() %>%
          plot_parameter_estimates()
      },
      width = est_plot_width
    ) |>
      bindEvent(powerresultsglm())

    output$parameterestimatessurv = renderPlot(
      {
        plot_parameter_estimates = function(powerresults) {
          x = lcb = ucb = vals = type = ylab = NULL
          if (!is.null(attr(powerresults, "estimates"))) {
            ests = apply(
              attr(powerresults, "estimates"),
              2,
              quantile,
              c(0.05, 0.5, 0.95),
              na.rm = TRUE
            )
            truth = attr(powerresults, "anticoef")
            if (isolate(input$distribution) == "exponential") {
              ests = exp(ests)
              truth = exp(truth)
            }
            censorpt = isolate(input$censorpoint)
            extra_gg = list()
            if (!is.na(censorpt)) {
              extra_gg[[1]] = geom_hline(
                yintercept = censorpt,
                linetype = "dashed",
                color = "black",
                alpha = 0.5,
                size = 1
              )
            }
            ylim = c(min(as.vector(ests)), max(as.vector(ests)))

            truth_df = data.frame(
              vals = c(truth, ests[2, ]),
              x = colnames(ests),
              type = c(
                rep("Truth", length(truth)),
                rep("Estimate", length(ests[2, ]))
              )
            )
            ests_ci_df = data.frame(
              lcb = ests[1, ],
              ucb = ests[3, ],
              x = colnames(ests)
            )
            break_vals = c(seq(ylim[1], ylim[2], length.out = 5), censorpt)
            nchar_max = max(nchar(sprintf(
              "%0.2f",
              seq(ylim[1], ylim[2], length.out = 5)
            )))
            break_labels = c(
              sprintf("%0.2f", seq(ylim[1], ylim[2], length.out = 5)),
              "Censored"
            )
            break_vals_order = order(break_vals, na.last = NA)
            break_vals = break_vals[break_vals_order]
            break_labels = break_labels[break_vals_order]

            ggplot() +
              extra_gg +
              geom_errorbar(
                data = ests_ci_df,
                aes(x = x, ymin = lcb, ymax = ucb),
                color = "black",
                width = 0.2
              ) +
              geom_point(data = truth_df, aes(x = x, y = vals, color = type)) +
              scale_color_manual("Type", values = c("red", "blue")) +
              scale_y_continuous(
                ylab,
                limits = range(break_vals),
                breaks = break_vals,
                labels = break_labels
              ) +
              scale_x_discrete("Parameters", labels = colnames(ests)) +
              theme_light() +
              theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                text = element_text(size = 16)
              ) +
              labs(
                title = "Simulated Parameter Estimates (5%-95% Confidence Intervals)"
              )
          }
        }
        powerresultssurv() %>%
          plot_parameter_estimates()
      },
      width = est_plot_width
    ) |>
      bindEvent(powerresultssurv())

    output$responsehistogram = renderPlot(
      {
        plot_response_histogram = function(powerresults) {
          if (!is.null(attr(powerresults, "estimates"))) {
            responses = as.vector(
              attr(powerresults, "estimates") %*%
                t(attr(powerresults, "modelmatrix"))
            )
            trueresponses = as.vector(
              attr(powerresults, "anticoef") %*%
                t(attr(powerresults, "modelmatrix"))
            )
            filtered_string = ""
            if (isolate(input$glmfamily) == "exponential") {
              #Filter out extreme values
              mad_trueresp = 20 * max(exp(trueresponses))
              num_filtered = sum(exp(responses) > mad_trueresp, na.rm = TRUE) +
                sum(is.na(exp(responses)))
              responses = responses[exp(responses) < mad_trueresp]
              trueresponses = trueresponses[exp(trueresponses) < mad_trueresp]
              filtered_string = sprintf(
                " (%g extreme outliers/NA values removed)",
                num_filtered
              )
            }
            widths = hist(trueresponses, plot = FALSE)$counts
            widths = widths[widths != 0]
            widths = sqrt(widths)
            uniquevalues = length(table(responses))
            breakvalues = ifelse(
              uniquevalues < isolate(input$nsim) * isolate(input$trials) / 10,
              uniquevalues,
              isolate(input$nsim) * isolate(input$trials) / 10
            )
            bin_values = 100
            if (isolate(input$glmfamily) == "binomial") {
              responses = exp(responses) / (1 + exp(responses))
              trueresponses = exp(trueresponses) / (1 + exp(trueresponses))
              data_response = data.frame(responses = responses)
              data_trueresponse = data.frame(trueresponses = trueresponses)
              ggplot() +
                geom_histogram(
                  data = data_response,
                  aes(x = responses),
                  bins = bin_values,
                  fill = "red"
                ) +
                geom_vline(
                  data = data_trueresponse,
                  aes(xintercept = trueresponses),
                  color = "blue",
                  alpha = 0.4,
                  linewidth = 2
                ) +
                scale_x_continuous(
                  "Response Estimates (Probability)",
                  limits = c(0, 1)
                ) +
                scale_y_continuous(expand = c(0, 0)) +
                theme_light() +
                theme(text = element_text(size = 24))
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
              # hist(responses, breaks = breakvalues, xlab = "Response (Probability)", main = "Distribution of Simulated Response Estimates", xlim = c(0, 1))
              # legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
              # grid(nx = NA, ny = NULL)
              # hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses Estimates", xlab = "Response", ylab = "Count", col = "red", border = "red")
              # abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
            } else if (isolate(input$glmfamily) == "poisson") {
              responses = exp(responses)
              trueresponses = exp(trueresponses)
              data_response = data.frame(responses = responses)
              data_trueresponse = data.frame(
                trueresponses = unique(trueresponses)
              )
              ggplot() +
                geom_histogram(
                  data = data_response,
                  aes(x = responses),
                  bins = bin_values,
                  fill = "red"
                ) +
                geom_vline(
                  data = data_trueresponse,
                  aes(xintercept = trueresponses),
                  color = "blue",
                  alpha = 0.4,
                  linewidth = 2
                ) +
                scale_x_continuous("Response Estimates") +
                scale_y_continuous(expand = c(0, 0)) +
                theme_light() +
                theme(text = element_text(size = 24))
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
              # hist(responses, breaks = breakvalues, xlab = "Response", main = "Distribution of Simulated Response Estimates",
              # xlim = c(ifelse(is.na(input$estimatesxminglm), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminglm),
              #          ifelse(is.na(input$estimatesxmaxglm), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxglm)), col = "red", border = "red")
              # legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
              # grid(nx = NA, ny = NULL)
              # hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses ", xlab = "Response", ylab = "Count", col = "red", border = "red")
              # abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
            } else if (isolate(input$glmfamily) == "exponential") {
              responses = exp(responses)
              trueresponses = exp(trueresponses)
              data_response = data.frame(responses = responses)
              data_trueresponse = data.frame(
                trueresponses = unique(trueresponses)
              )
              ggplot() +
                geom_histogram(
                  data = data_response,
                  aes(x = responses),
                  bins = bin_values,
                  fill = "red"
                ) +
                geom_vline(
                  data = data_trueresponse,
                  aes(xintercept = trueresponses),
                  color = "blue",
                  alpha = 0.4,
                  linewidth = 2
                ) +
                scale_x_continuous(sprintf(
                  "Response Estimates%s",
                  filtered_string
                )) +
                scale_y_continuous(expand = c(0, 0)) +
                theme_light() +
                theme(text = element_text(size = 24))

              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
              # hist(responses, breaks = breakvalues, xlab = "Response", main = "Distribution of Simulated Response Estimates",
              #      xlim = c(ifelse(is.na(input$estimatesxminglm), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminglm), ifelse(is.na(input$estimatesxmaxglm), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxglm)), col = "red", border = "red")
              # legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
              # grid(nx = NA, ny = NULL)
              # hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses", xlab = "Response", ylab = "Count", col = "red", border = "red")
              # abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
            } else if (isolate(input$glmfamily) == "gaussian") {
              data_response = data.frame(responses = responses)
              data_trueresponse = data.frame(
                trueresponses = unique(trueresponses)
              )
              ggplot() +
                geom_histogram(
                  data = data_response,
                  aes(x = responses),
                  bins = bin_values,
                  fill = "red"
                ) +
                geom_vline(
                  data = data_trueresponse,
                  aes(xintercept = trueresponses),
                  color = "blue",
                  alpha = 0.4,
                  linewidth = 2
                ) +
                scale_x_continuous("Response Estimates") +
                scale_y_continuous(expand = c(0, 0)) +
                theme_light() +
                theme(text = element_text(size = 24))

              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
              # hist(responses, breaks = breakvalues, xlab = "Response", main = "Distribution of Simulated Response Estimates", xlim = c(ifelse(is.na(input$estimatesxminglm), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminglm), ifelse(is.na(input$estimatesxmaxglm), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxglm)), col = "red", border = "red")
              # legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
              # grid(nx = NA, ny = NULL)
              # hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses", xlab = "Response", ylab = "Count", col = "red", border = "red")
              # abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
            }
          }
        }
        powerresultsglm() %>%
          plot_response_histogram()
      },
      width = est_plot_width
    ) |>
      bindEvent(powerresultsglm())

    output$responsehistogramsurv = renderPlot(
      {
        plot_response_histogram = function(powerresults) {
          if (!is.null(attr(powerresults, "estimates"))) {
            responses = as.vector(
              attr(powerresults, "estimates") %*%
                t(attr(powerresults, "modelmatrix"))
            )
            trueresponses = as.vector(
              attr(powerresults, "anticoef") %*%
                t(attr(powerresults, "modelmatrix"))
            )
            filtered_string = ""
            bin_values = 100
            if (isolate(input$distribution) == "exponential") {
              #Filter out extreme values
              mad_trueresp = 10 * max(exp(trueresponses), na.rm = TRUE)
              num_filtered = sum(exp(responses) > mad_trueresp, na.rm = TRUE)
              responses = responses[exp(responses) < mad_trueresp]
              trueresponses = trueresponses[exp(trueresponses) < mad_trueresp]
              filtered_string = sprintf(
                " (%g extreme outliers removed)",
                num_filtered
              )

              responses = exp(responses)
              trueresponses = exp(trueresponses)
              data_response = data.frame(responses = responses)
              data_trueresponse = data.frame(
                trueresponses = unique(trueresponses)
              )
              ggplot() +
                geom_histogram(
                  data = data_response,
                  aes(x = responses),
                  bins = bin_values,
                  fill = "red"
                ) +
                geom_vline(
                  data = data_trueresponse,
                  aes(xintercept = trueresponses),
                  color = "blue",
                  alpha = 0.4,
                  linewidth = 2
                ) +
                scale_x_continuous(sprintf(
                  "Simulated Response Estimates%s",
                  filtered_string
                )) +
                scale_y_continuous(expand = c(0, 0)) +
                theme_light() +
                theme(text = element_text(size = 24))
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
              # hist(responses, breaks = breakvalues, xlab = "Response", main = sprintf("Distribution of Simulated Response Estimates%s", filtered_string),
              # xlim = c(ifelse(is.na(input$estimatesxminsurv), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminsurv), ifelse(is.na(input$estimatesxmaxsurv), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxsurv)), col = "red", border = "red")
              # legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
              # grid(nx = NA, ny = NULL)
              # hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses", xlab = "Response", ylab = "Count", col = "red", border = "red")
              # abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
            }
            if (isolate(input$distribution) %in% c("gaussian", "lognormal")) {
              mad_trueresp = 10 * max(abs(trueresponses), na.rm = TRUE)
              num_filtered = sum(abs(responses) > mad_trueresp, na.rm = TRUE)
              responses = responses[abs(responses) < mad_trueresp]
              filtered_string = sprintf(
                " (%g extreme outliers removed)",
                num_filtered
              )

              data_response = data.frame(responses = responses)
              data_trueresponse = data.frame(
                trueresponses = unique(trueresponses)
              )
              ggplot() +
                geom_histogram(
                  data = data_response,
                  aes(x = responses),
                  bins = bin_values,
                  fill = "red"
                ) +
                geom_vline(
                  data = data_trueresponse,
                  aes(xintercept = trueresponses),
                  color = "blue",
                  alpha = 0.4,
                  linewidth = 2
                ) +
                scale_x_continuous(sprintf(
                  "Simulated Response Estimates%s",
                  filtered_string
                )) +
                scale_y_continuous(expand = c(0, 0)) +
                theme_light() +
                theme(text = element_text(size = 24))
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
              # hist(responses, breaks = breakvalues, xlab = "Response", main = "Distribution of Simulated Response Estimates (from survival analysis)", xlim = c(ifelse(is.na(input$estimatesxminsurv), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminsurv), ifelse(is.na(input$estimatesxmaxsurv), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxsurv)), col = "red", border = "red")
              # legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
              # par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
              # grid(nx = NA, ny = NULL)
              # hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses", xlab = "Response", ylab = "Count", col = "red", border = "red")
              # abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
            }
          }
        }
        powerresultssurv() %>%
          plot_response_histogram()
      },
      width = est_plot_width
    ) |>
      bindEvent(powerresultssurv())

    output$separationwarning = renderText({
      output_separation_warning = function(powerresults) {
        likelyseparation = FALSE
        if (input$evaltype == "glm" && input$glmfamily == "binomial") {
          if (!is.null(attr(powerresults, "pvals"))) {
            pvalmat = attr(powerresults, "pvals")
            for (i in 2:ncol(pvalmat)) {
              pvalcount = hist(
                pvalmat[, i],
                breaks = seq(0, 1, 0.05),
                plot = FALSE
              )
              likelyseparation = likelyseparation ||
                (all(pvalcount$count[20] > pvalcount$count[17:19]) &&
                  pvalcount$count[20] > isolate(input$nsim) / 15)
            }
          }
        }
        if (likelyseparation && !getOption("in_skpr_test_environment", TRUE)) {
          showNotification(
            "Partial or complete separation likely detected in the binomial Monte Carlo simulation. Increase the number of runs in the design or decrease the number of model parameters to improve power.",
            type = "warning",
            duration = 10
          )
        }
      }
      if (evaluationtype() == "glm") {
        powerresultsglm() %>%
          output_separation_warning()
      }
      ""
    }) |>
      bindEvent(powerresultsglm())

    any_htc = reactive({
      has_htc = FALSE
      for (i in seq_len(input$numberfactors)) {
        if (!is.null(input[[sprintf("blockdepth%i", i)]])) {
          if (input[[sprintf("blockdepth%i", i)]] == "htc") {
            has_htc = TRUE
          }
        }
      }
      has_htc
    })

    factor_input_cache = reactiveValues()

    ui_elements = reactive({
      on.exit(
        {
          shinyjs::enable("evalbutton")
          shinyjs::enable("submitbutton")
        },
        add = TRUE
      )
      req(factor_input_cache, cancelOutput = TRUE)
      ui_elements_list = list()
      if (input$numberfactors > 1) {
        for (i in seq_len(input$numberfactors)[-1]) {
          ui_elements_list[[i - 1]] = generate_factor_input_panel(
            i,
            reactiveValuesToList(factor_input_cache)
          )
        }
      }
      do.call(tagList, ui_elements_list)
    }) |>
      bindEvent(updated_ui_defaults())

    current_design_valid = reactiveVal(FALSE)
    valid_code_pane = reactiveVal(TRUE)

    updated_ui_defaults = reactive({
      current_design_valid(FALSE)
      valid_code_pane(FALSE)
      shinyjs::disable("submitbutton")
      shinyjs::disable("evalbutton")
      for (i in seq_len(input$numberfactors)) {
        factorname_n = sprintf("factorname%i", i)
        factortype_n = sprintf("factortype%i", i)
        numericlow_n = sprintf("numericlow%i", i)
        numerichigh_n = sprintf("numerichigh%i", i)
        numericlength_n = sprintf("numericlength%i", i)
        disclevels_n = sprintf("disclevels%i", i)
        levels_n = sprintf("levels%i", i)
        blockdepth_n = sprintf("blockdepth%i", i)

        if (is.null(factor_input_cache[[factorname_n]])) {
          factor_input_cache[[factorname_n]] = sprintf("X%i", i)
          factor_input_cache[[factortype_n]] = "Continuous"
          factor_input_cache[[numericlow_n]] = -1
          factor_input_cache[[numerichigh_n]] = 1
          factor_input_cache[[numericlength_n]] = 3
          factor_input_cache[[disclevels_n]] = ""
          factor_input_cache[[levels_n]] = "a, b, c"
          factor_input_cache[[blockdepth_n]] = "Easy"
        } else {
          factor_input_cache[[factorname_n]] = input[[factorname_n]]
          factor_input_cache[[factortype_n]] = input[[factortype_n]]
          factor_input_cache[[numericlow_n]] = input[[numericlow_n]]
          factor_input_cache[[numerichigh_n]] = input[[numerichigh_n]]
          factor_input_cache[[numericlength_n]] = input[[numericlength_n]]
          factor_input_cache[[disclevels_n]] = input[[disclevels_n]]
          factor_input_cache[[levels_n]] = input[[levels_n]]
          factor_input_cache[[blockdepth_n]] = input[[blockdepth_n]]
        }
      }
      input$numberfactors
    }) |>
      bindEvent(input$numberfactors) |>
      debounce(500)

    output$additional_factors = renderUI({
      on.exit(
        valid_code_pane(TRUE),
        add = TRUE
      )
      ui_elements()
    })
    output$block_panel = renderUI({
      generate_block_panel(any_htc())
    })
    output$optimality_results = renderUI({
      generate_optimality_results(any_htc())
    })

    observeEvent(input$evalbutton, {
      updateNavbarPage(session, "results_panels", selected = "eval")
    })

    observeEvent(input$submitbutton, {
      updateNavbarPage(session, "results_panels", selected = "design")
    })

    displayed_design_number_factors = reactive({
      input$numberfactors
    }) |>
      bindEvent(input$submitbutton)

    output$displayed_design_number_factors = reactive({
      displayed_design_number_factors()
    })

    observeEvent(
      input$tutorial,
      rintrojs::introjs(
        session,
        options = list("showProgress" = "true", "showBullets" = "false"),
        events = list(
          "onchange" = I(
            "
                                          if (this._currentStep==0) {
                                          $('a[data-value=\"Advanced\"]').removeClass('active');
                                          $('a[data-value=\"Power\"]').removeClass('active');
                                          $('a[data-value=\"Basic\"]').addClass('active');
                                          $('a[data-value=\"Basic\"]').trigger('click');
                                          }
                                          if (this._currentStep==5) {
                                          $('a[data-value=\"Power\"]').removeClass('active');
                                          $('a[data-value=\"Basic\"]').removeClass('active');
                                          $('a[data-value=\"Advanced\"]').addClass('active');
                                          $('a[data-value=\"Advanced\"]').trigger('click');
                                          }
                                          if (this._currentStep==13) {
                                          $('a[data-value=\"Advanced\"]').removeClass('active');
                                          $('a[data-value=\"Power\"]').addClass('active');
                                          $('a[data-value=\"Power\"]').trigger('click');
                                          }
                                          if (this._currentStep==17) {
                                          $('input[value=\"glm\"]').trigger('click');
                                          }
                                          if (this._currentStep==21) {
                                          $('input[value=\"surv\"]').trigger('click');
                                          }
                                          if (this._currentStep==24) {
                                          $('a[data-value=\"Design Evaluation\"]').removeClass('active');
                                          $('a[data-value=\"Generating Code\"]').removeClass('active');
                                          $('a[data-value=\"Design\"]').addClass('active');
                                          $('a[data-value=\"Design\"]').trigger('click');
                                          $('#evaltype').val('glm');
                                          Shiny.onInputChange('evaltype', 'glm');
                                          $('#numberfactors').val('3');
                                          Shiny.onInputChange('numberfactors', 3);
                                          $('#trials').val('12');
                                          Shiny.onInputChange('trials', 12);
                                          $('#submitbutton').trigger('click');
                                          $('#evalbutton').trigger('click');
                                          }
                                          if (this._currentStep==25) {
                                          $('#evalbutton').trigger('click');
                                          $('a[data-value=\"Design\"]').removeClass('active');
                                          $('a[data-value=\"Design Evaluation\"]').addClass('active');
                                          $('a[data-value=\"Design Evaluation\"]').trigger('click');
                                          }
                                          if (this._currentStep==31) {
                                          $('a[data-value=\"Design Evaluation\"]').removeClass('active');
                                          $('a[data-value=\"Generating Code\"]').addClass('active');
                                          $('a[data-value=\"Generating Code\"]').trigger('click');
                                          }"
          )
        )
      )
    )
    outputOptions(output, "separationwarning", suspendWhenHidden = FALSE)
  }
  if (return_app) {
    return(shinyApp(ui, server, enableBookmarking = "url"))
  }
  if (browser) {
    runGadget(
      shinyApp(ui, server, enableBookmarking = "url"),
      viewer = browserViewer()
    )
  } else {
    runGadget(
      shinyApp(ui, server, enableBookmarking = "url"),
      viewer = dialogViewer(dialogName = "skprGUI", width = 1400, height = 1200)
    )
  }
}
# nocov end
