#'@title Graphical User Interface for skpr
#'
#'@description skprGUI provides a graphical user interface to skpr, within R Studio.
#'
#'@param inputValue1 Required by Shiny
#'@param inputValue2 Required by Shiny
#'
#'@import future
#'@export
#'@examples
#'#Type `skprGUIserver()` to begin
#'
# nocov start
skprGUIserver = function(inputValue1, inputValue2) {
  check_for_suggest_packages(c("shiny","shinythemes","shinyjs","gt","rintrojs"))

  panelstyle = "background-color: rgba(86, 96, 133, 0.3);
                border-radius: 15px;
                -webkit-box-shadow: inset 0px 0px 10px 4px rgba(41, 49, 83, 0.42);
                box-shadow: inset 0px 0px 10px 2px rgba(41, 49, 83, 0.42);
                padding-top: 15px;
                padding-bottom: 10px;
                color: rgb(255, 255, 255);
                border: 0px;"
  b64 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFUAAAAnCAYAAAB+HwSQAAAACXBIWXMAAC4jAAAuIwF4pT92AAAAB3RJTUUH5AEKEAsxvdBAYQAAAAd0RVh0QXV0aG9yAKmuzEgAAAAMdEVYdERlc2NyaXB0aW9uABMJISMAAAAKdEVYdENvcHlyaWdodACsD8w6AAAADnRFWHRDcmVhdGlvbiB0aW1lADX3DwkAAAAJdEVYdFNvZnR3YXJlAF1w/zoAAAALdEVYdERpc2NsYWltZXIAt8C0jwAAAAh0RVh0V2FybmluZwDAG+aHAAAAB3RFWHRTb3VyY2UA9f+D6wAAAAh0RVh0Q29tbWVudAD2zJa/AAAABnRFWHRUaXRsZQCo7tInAAAFW0lEQVRoge2aa6hVRRTHf+vejq/Uq6WZVj6SIEoiSkQpIorMCCSUwkQjtYdpBL3UrJAQLC0lzMgyqC/RBQ2KEAn6UFEISpJBaZpW+KCXmY/ypl7/fZh98nicmb3POfvaRu4fBvaZtWbN2uvMrL1mrQF4BhBwPNAE/Aj0lwTwUNLXHhmTpR0DDgN7gI1Aa6LLbcCFkmi0ASVgc8r7ld/xF2BgLvMmkz+ZCPa1DUCvKmUnJkYNjWm0tQFrgbE5GLYv8GXKfNuAAXkYtNKoV0YmfMWjaG/gUAcatbK9S7JLGjDsmylzfJaXQSXRhEN3wih5+mL8eWMSsN7MrqhnsJmVgJtS2EaZ2eX1yPehKZ2lLpzI0GrBcOBjMxtahy6TgGEpPF2B+XXI9uKcvARV4AtgGmApfF2APsAlwDXAWOCqCP9AYI2ZjZZ0PIsiZmbAvCy8wN1mtlDS9oz8YSQ+ZyRhf7PS46MGEPap6xrwfXcCuyK6CJhfg7wJKbKq29t5+tQ80VzvQEmrgTHAtxG2uWbWL6PIWrf0ZDMbXuOY09BRPrVuSNoNjAcOBlh6A9PT5JjZOOBaD2kXMCcwrEQOvrVwRgWQtAN4PsIyOYOYpwP9qyW9COwI0KeYWdqHLYpCGjXB68CBAG1EbJua2Q3A9R7SUeDV5Hl5YHgX4KmsSvpQWKNK2g98GiA349/aZYRW6RpJO5Pnt4DfA3z3mNmQdC39KKxRE6yP0LyHATMbiQvPfFhSfpB0CFgV4OtK9lDsNBTdqN9HaBcH+kOr9CNJm6v6VgBHAvz3mtngmHIhFN2o+yK0luoOMxuBixx8eKG6Q9JeXG7Bh27A3DQFfSi6UY9GaL6cxDz877RB0icBOcsIH5unmVloRwRRdKP6DFfGscofSTRwV4B3SaAfSd8A6wLk7oRj2iCKbtTzI7TqcGsO/j/hK0nvpcyzAJcf9mGGmQ1KGX8KOiKhkicujdD2lh+SLTo1wLfWzC4j/q6HgK24vHI1euCS+I/GVT2Joht1dIS2teL5McI53icIRwRlnMCVVUK438wWS/o5RQ5Q4O1vZr2AGwPkE7gSCWbWH7gvIqprhumacCepEM7F/TmZUFij4pIm5wVoW3B1JYBHgF5nQJ8HzeyCLIyFNKqZXQQ8G2FplSQzawFmRfiyVCCyViR6Ao9n0b9wPjXJlX5A+Mv/F66QBzAT/2o+DNyCOzykVSAqIeABwlt9ppm9JOm3uJRiZf5vB7ZHdBGwIOHtgYsAfDwvN6BDb+CPyPyL0mR0xErtmRToYitEuJiyBRgMXA3cCoxKkb0JWJQ8T8fVrarRBizNrm6VYtJBM1tB2P3MMrOlksJH6A5Yqe3AP7gjZqzVehljNzAkmb8E/BDgW9VojQnnev6M6LLwTNeoyuFJKaXVMvd24GZJPyW/pwBDPXxtRI6kWZGswtciLA+bWd8QsfxibREBxzx9oXRZR6AVGCPpu4q+UGb+c+VRYnZ4g/DRtQ+x6CRZ7rG7VJuAlqrtMTHCn0fz3qXCBfLLI+O2AYPyKDMDd2TQcypg1WMNmA0sJhyjlYCvgXGS9pvZZFxyt9GPnHBHwyO45MjexCgbcXebdlYyJ9vtfeA64O+IrruB8ZK21KuYmc0GniN+ymrC2eAdSTNOGQ/0S5QJLXVwCds9ktqTl+uB3y3UgrJR2ySluhMza8bdZjmSjI3p+qukmEtLm6s/zqBp79gMNMuV1U+OT5Z6J3KEUduJoxN+mKT/3Kfh/GW3/0+fswbLJK0EZ9TO/Z8PDgDDJe1rIvwl7URtaMFFDBjOqGfyZvTZjHZgmOFu13X61HzQDHz4LwBKRJdWuineAAAAAElFTkSuQmCC"

  ui = function(request) {
    shiny::fluidPage(theme = shinythemes::shinytheme("yeti"),
              shinyjs::useShinyjs(),
                 rintrojs::introjsUI(),
                 shiny::HTML("<style> table {font-size: 14px;}
                      .btn2 {
                      color: #fff;
                      border-color: rgba(46, 109, 164, 0);
                      background: linear-gradient(to bottom, rgb(64, 108, 221) 0%, rgb(107, 167, 223) 100%);
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
                      .btn2:hover {
                      color: #fff;
                      border-color: rgb(48, 163, 43);
                      background: linear-gradient(to bottom, rgb(78, 206, 114) 0%, rgb(71, 146, 95) 100%);
                      border-radius: 11px;
                      -webkit-box-shadow: 0px 2px 0px 0px rgb(59, 76, 145);
                      box-shadow: 0px 2px 0px 0px rgb(59, 76, 145);
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
                      .irs-grid-text {color: rgb(0, 0, 0);}</style>"),
                 shiny::sidebarLayout(
                   shiny::sidebarPanel(shiny::tags$style(".well {background-color:#a1b0da;
                                           border: 1px solid #a1b0da;
                                           border-radius: 13px;
                                           -webkit-box-shadow: 0px 0px 10px 5px rgba(0, 0, 0, 0.15);
                                           box-shadow: 0px 0px 10px 5px rgba(0, 0, 0, 0.15);}"),
                                shiny::HTML("<h1 style='margin-top: 0px;'>skpr<strong style='color: black;'>GUI</strong></h1>"),
                                shiny::hr(),
                                rintrojs::introBox(shiny::fluidRow(
                                  shiny::column(width = 6,
                                         shiny::actionButton("submitbutton", shiny::HTML("<strong>Generate <br>Design</strong>"),
                                                      class = "btn2")
                                  ),
                                  shiny::column(width = 6,
                                         shiny::actionButton("evalbutton", shiny::HTML("<strong>Evaluate <br>Design</strong>"),
                                                      class = "btn2")
                                  )
                                ), data.step = 1, data.intro = "<h3><center>Welcome to skpr!</h3></center> This tutorial will walk you through all of the features of the GUI and teach you how to create and analyze an experimental design. All features seen in the GUI can be easily recreated in the console, and skpr provides the full script used to do that, based on your inputs. Additional advanced capabilities not available in the GUI can be accessed via the code. <b>Let's get started!</b> <br><br>Click these buttons to generate a new design, or re-run a new design evaluation with updated parameters."),
                                shiny::tabsetPanel(
                                  shiny::tabPanel(
                                    "Basic",
                                    rintrojs::introBox(shiny::numericInput(inputId = "trials",
                                                          12, label = "Trials"), data.step = 2, data.intro = "This is the number of runs in the experiment."),
                                    rintrojs::introBox(shiny::textInput(inputId = "model",
                                                       "~.", label = "Model"), data.step = 3, data.intro = "This is the model. <br><br> <b>~.</b> produces a linear model for all terms with no interactions. <br><br> Interactions can be added with the colon operator: <br><br> <b>~X1 + X2 + X1:X2</b> <br><br> and quadratic effects with an I() (as in India): <br><br><b>~X1 + X2 + I(X1^2)</b>."),
                                    shiny::conditionalPanel(condition = "(input.blockdepth1 == 'htc') ||
                                                                  (input.blockdepth2 == 'htc' && input.numberfactors > 1) ||
                                                                  (input.blockdepth3 == 'htc' && input.numberfactors > 2) ||
                                                                  (input.blockdepth4 == 'htc' && input.numberfactors > 3) ||
                                                                  (input.blockdepth5 == 'htc' && input.numberfactors > 4) ||
                                                                  (input.blockdepth6 == 'htc' && input.numberfactors > 5)",
                                                     shiny::fluidRow(
                                                       shiny::column(width = 12, shiny::numericInput(inputId = "numberblocks",
                                                                                    4, label = "Number of blocks"))
                                                     )
                                    ),
                                    shiny::conditionalPanel(condition = "input.numberfactors == 6",
                                                     shiny::fluidRow(
                                                       shiny::column(width = 12,
                                                              shiny::HTML("<p style=\"color: #000;\">skprGUI only supports up to 6 factors. Alter the generated code to add more.</p>")
                                                       )
                                                     )
                                    ),
                                    rintrojs::introBox(shiny::numericInput(inputId = "numberfactors",
                                                          min = 1, max = 6, 1, label = "Number of Factors"), data.step = 4, data.intro = "This is the number of factors in the experiment. skprGUI supports up to 6 factors, but the underlying code supports any number of factors by calling the code directly. If you require more factors, use the generating code as a template and add more terms to the candidate set."),
                                    shiny::br(),
                                    rintrojs::introBox(shiny::wellPanel(style = panelstyle,
                                                       shiny::h3("Factor 1"),
                                                       shiny::fluidRow(
                                                         shiny::column(width = 5,
                                                                shiny::selectInput(inputId = "blockdepth1",
                                                                            choices = list("Easy" = "etc", "Hard" = "htc"),
                                                                            label = "Changes")
                                                         ),
                                                         shiny::column(width = 7,
                                                                shiny::selectInput(inputId = "factortype1",
                                                                            choices = list("Continuous" = "numeric", "Categorical" = "cat", "Discrete Numeric" = "discnum"),
                                                                            label = "Type")
                                                         )
                                                       ),
                                                       shiny::fluidRow(
                                                         shiny::column(width = 12,
                                                                shiny::textInput(inputId = "factorname1",
                                                                          value = "X1",
                                                                          label = "Name")
                                                         )
                                                       ),
                                                       shiny::conditionalPanel(
                                                         condition = "input.factortype1 == \'numeric\'",
                                                         shiny::fluidRow(
                                                           shiny::column(width = 4,
                                                                  shiny::numericInput(inputId = "numericlow1",
                                                                               value = -1,
                                                                               label = "Low")
                                                           ),
                                                           shiny::column(width = 4,
                                                                  shiny::numericInput(inputId = "numerichigh1",
                                                                               value = 1,
                                                                               label = "High")
                                                           ),
                                                           shiny::column(width = 4,
                                                                  shiny::numericInput(inputId = "numericlength1",
                                                                               value = 3,
                                                                               min = 2,
                                                                               step = 1,
                                                                               label = "Breaks")
                                                           )
                                                         )
                                                       ),
                                                       shiny::conditionalPanel(
                                                         condition = "input.factortype1 == \'discnum\'",
                                                         shiny::fluidRow(
                                                           shiny::column(width = 12,
                                                                  shiny::textInput(inputId = "disclevels1",
                                                                            value = "",
                                                                            label = "Levels (separate with commas)")
                                                           )
                                                         )
                                                       ),
                                                       shiny::conditionalPanel(
                                                         condition = "input.factortype1 == \'cat\'",
                                                         shiny::fluidRow(
                                                           shiny::column(width = 12,
                                                                  shiny::textInput(inputId = "levels1",
                                                                            value = "",
                                                                            label = "Levels (separate with commas)")
                                                           )
                                                         )
                                                       )
                                    ), data.step = 5, data.intro = "This pane allows you to change the factor type, specify categorical and discrete numeric levels, and make factors hard-to-change. If numeric, specify the highest and lowest values and the number of breaks between. If categorical or discrete numeric, specify levels separated by commas."),
                                    shiny::conditionalPanel(
                                      condition = "input.numberfactors > 1",
                                      shiny::wellPanel(style = panelstyle,
                                                shiny::h3("Factor 2"),
                                                shiny::fluidRow(
                                                  shiny::column(width = 5,
                                                         shiny::selectInput(inputId = "blockdepth2",
                                                                     choices = list("Easy" = "etc", "Hard" = "htc"),
                                                                     label = "Changes")
                                                  ),
                                                  shiny::column(width = 7,
                                                         shiny::selectInput(inputId = "factortype2",
                                                                     choices = list("Continuous" = "numeric", "Categorical" = "cat", "Discrete Numeric" = "discnum"),
                                                                     label = "Type")
                                                  )
                                                ),
                                                shiny::fluidRow(
                                                  shiny::column(width = 12,
                                                         shiny::textInput(inputId = "factorname2",
                                                                   value = "X2",
                                                                   label = "Name")
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype2 == \'numeric\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlow2",
                                                                        value = -1,
                                                                        label = "Low")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numerichigh2",
                                                                        value = 1,
                                                                        label = "High")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlength2",
                                                                        value = 3,
                                                                        min = 2,
                                                                        step = 1,
                                                                        label = "Breaks")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype2 == \'discnum\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "disclevels2",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype2 == \'cat\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "levels2",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                )
                                      )
                                    ),
                                    shiny::conditionalPanel(
                                      condition = "input.numberfactors > 2",
                                      shiny::wellPanel(style = panelstyle,
                                                shiny::h3("Factor 3"),
                                                shiny::fluidRow(
                                                  shiny::column(width = 5,
                                                         shiny::selectInput(inputId = "blockdepth3",
                                                                     choices = list("Easy" = "etc", "Hard" = "htc"),
                                                                     label = "Changes")
                                                  ),
                                                  shiny::column(width = 7,
                                                         shiny::selectInput(inputId = "factortype3",
                                                                     choices = list("Continuous" = "numeric", "Categorical" = "cat", "Discrete Numeric" = "discnum"),
                                                                     label = "Type")
                                                  )
                                                ),
                                                shiny::fluidRow(
                                                  shiny::column(width = 12,
                                                         shiny::textInput(inputId = "factorname3",
                                                                   value = "X3",
                                                                   label = "Name")
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype3 == \'numeric\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlow3",
                                                                        value = -1,
                                                                        label = "Low")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numerichigh3",
                                                                        value = 1,
                                                                        label = "High")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlength3",
                                                                        value = 3,
                                                                        min = 2,
                                                                        step = 1,
                                                                        label = "Breaks")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype3 == \'discnum\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "disclevels3",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype3 == \'cat\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "levels3",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                )
                                      )
                                    ),
                                    shiny::conditionalPanel(
                                      condition = "input.numberfactors > 3",
                                      shiny::wellPanel(style = panelstyle,
                                                shiny::h3("Factor 4"),
                                                shiny::fluidRow(
                                                  shiny::column(width = 5,
                                                         shiny::selectInput(inputId = "blockdepth4",
                                                                     choices = list("Easy" = "etc", "Hard" = "htc"),
                                                                     label = "Changes")
                                                  ),
                                                  shiny::column(width = 7,
                                                         shiny::selectInput(inputId = "factortype4",
                                                                     choices = list("Continuous" = "numeric", "Categorical" = "cat", "Discrete Numeric" = "discnum"),
                                                                     label = "Type")
                                                  )
                                                ),
                                                shiny::fluidRow(
                                                  shiny::column(width = 12,
                                                         shiny::textInput(inputId = "factorname4",
                                                                   value = "X4",
                                                                   label = "Name")
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype4 == \'numeric\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlow4",
                                                                        value = -1,
                                                                        label = "Low")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numerichigh4",
                                                                        value = 1,
                                                                        label = "High")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlength4",
                                                                        value = 3,
                                                                        min = 2,
                                                                        step = 1,
                                                                        label = "Breaks")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype4 == \'discnum\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "disclevels4",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype4 == \'cat\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "levels4",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                )
                                      )
                                    ),
                                    shiny::conditionalPanel(
                                      condition = "input.numberfactors > 4",
                                      shiny::wellPanel(style = panelstyle,
                                                shiny::h3("Factor 5"),
                                                shiny::fluidRow(
                                                  shiny::column(width = 5,
                                                         shiny::selectInput(inputId = "blockdepth5",
                                                                     choices = list("Easy" = "etc", "Hard" = "htc"),
                                                                     label = "Changes")
                                                  ),
                                                  shiny::column(width = 7,
                                                         shiny::selectInput(inputId = "factortype5",
                                                                     choices = list("Continuous" = "numeric", "Categorical" = "cat", "Discrete Numeric" = "discnum"),
                                                                     label = "Type")
                                                  )
                                                ),
                                                shiny::fluidRow(
                                                  shiny::column(width = 12,
                                                         shiny::textInput(inputId = "factorname5",
                                                                   value = "X5",
                                                                   label = "Name")
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype5 == \'numeric\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlow5",
                                                                        value = -1,
                                                                        label = "Low")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numerichigh5",
                                                                        value = 1,
                                                                        label = "High")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlength5",
                                                                        value = 3,
                                                                        min = 2,
                                                                        step = 1,
                                                                        label = "Breaks")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype5 == \'discnum\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "disclevels5",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype5 == \'cat\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "levels5",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                )
                                      )
                                    ),
                                    shiny::conditionalPanel(
                                      condition = "input.numberfactors > 5",
                                      shiny::wellPanel(style = panelstyle,
                                                shiny::h3("Factor 6"),
                                                shiny::fluidRow(
                                                  shiny::column(width = 5,
                                                         shiny::selectInput(inputId = "blockdepth6",
                                                                     choices = list("Easy" = "etc", "Hard" = "htc"),
                                                                     label = "Changes")
                                                  ),
                                                  shiny::column(width = 7,
                                                         shiny::selectInput(inputId = "factortype6",
                                                                     choices = list("Continuous" = "numeric", "Categorical" = "cat", "Discrete Numeric" = "discnum"),
                                                                     label = "Type")
                                                  )
                                                ),
                                                shiny::fluidRow(
                                                  shiny::column(width = 12,
                                                         shiny::textInput(inputId = "factorname6",
                                                                   value = "X6",
                                                                   label = "Name")
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype6 == \'numeric\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlow6",
                                                                        value = -1,
                                                                        label = "Low")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numerichigh6",
                                                                        value = 1,
                                                                        label = "High")
                                                    ),
                                                    shiny::column(width = 4,
                                                           shiny::numericInput(inputId = "numericlength6",
                                                                        value = 3,
                                                                        min = 2,
                                                                        step = 1,
                                                                        label = "Breaks")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype6 == \'discnum\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "disclevels6",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                ),
                                                shiny::conditionalPanel(
                                                  condition = "input.factortype6 == \'cat\'",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 12,
                                                           shiny::textInput(inputId = "levels6",
                                                                     value = "",
                                                                     label = "Levels (separate with commas)")
                                                    )
                                                  )
                                                )
                                      )
                                    )
                                  ),
                                  shiny::tabPanel("Advanced",
                                           rintrojs::introBox(shiny::selectInput(inputId = "optimality",
                                                                choices = c("D", "I", "A", "Alias", "G", "E", "T"),
                                                                label = "Optimality"), data.step = 6, data.intro = "Change the optimality criterion. If Alias-optimal selected, additional Alias-optimal specific options (minimum D-optimality and Alias-interaction level) will become available to change."),
                                           rintrojs::introBox(shiny::numericInput(inputId = "repeats",
                                                                 20, label = "Repeats"), data.step = 7, data.intro = "Changes the depth of the optimal design search. Increasing this will increase the probability that an optimal design is found."),
                                           rintrojs::introBox(shiny::numericInput(inputId = "varianceratio",
                                                                 1, label = "Variance Ratio"), data.step = 8, data.intro = "The ratio of the variance between whole plots and subplots for split-plot designs."),
                                           shiny::conditionalPanel(
                                             condition = "input.optimality == \'Alias\'",
                                             shiny::numericInput(inputId = "aliaspower",
                                                          min = 2, value = 2, label = "Alias Optimal Interaction Level"),
                                             sliderInput(inputId = "mindopt",
                                                         min = 0, max = 1, value = 0.8, label = "Minimum D Optimality")
                                           ),
                                           rintrojs::introBox(checkboxInput(inputId = "setseed",
                                                                  label = "Set Random Number Generator Seed",
                                                                  value = FALSE), data.step = 9, data.intro = "Set the random seed for both design generation and evaluation. This allows for completely reproducible designs and Monte Carlo simulations."),
                                           shiny::conditionalPanel(
                                             condition = "input.setseed",
                                             shiny::numericInput(inputId = "seed",
                                                          1, label = "Random Seed")
                                           ),
                                           rintrojs::introBox(checkboxInput(inputId = "parallel",
                                                                  label = "Parallel (disabled on server)",
                                                                  value = FALSE), data.step = 10, data.intro = "Use all available cores to compute design. Only set to true if the design search is taking >10 seconds to finish. Otherwise, the overhead in setting up the parallel computation outweighs the speed gains."),
                                           rintrojs::introBox(checkboxInput(inputId = "splitanalyzable",
                                                                  label = "Include Blocking Columns in Run Matrix",
                                                                  value = TRUE), data.step = 11, data.intro = "Convert row structure to blocking columns. This is required for analyzing the split-plot structure using REML."),
                                           rintrojs::introBox(checkboxInput(inputId = "detailedoutput",
                                                                  label = "Detailed Output",
                                                                  value = FALSE), data.step = 12, data.intro = "Outputs a tidy data frame of additional design information, including anticipated coefficients and design size."),
                                           rintrojs::introBox(checkboxInput(inputId = "advanceddiagnostics",
                                                                  label = "Advanced Design Diagnostics",
                                                                  value = TRUE), data.step = 13, data.intro = "Outputs additional information about the optimal search and advanced Monte Carlo information. This includes a list of all available optimal criteria, a plot of the computed optimal values during the search (useful for determining if the repeats argument should be increased), and a histogram of p-values for each parameter in Monte Carlo simulations."),
                                           shiny::selectInput(inputId = "colorchoice", choices = c("Default" = "D", "Magma" = "A", "Inferno" = "B", "Plasma" = "C", "None" = "none"), label = "Color")
                                  ),
                                  shiny::tabPanel("Power",
                                           rintrojs::introBox(rintrojs::introBox(rintrojs::introBox(radioButtons(inputId = "evaltype",
                                                                                   label = "Model Type",
                                                                                   choiceNames = c("Linear Model", "Generalized Linear Model", "Survival Model"),
                                                                                   choiceValues = c("lm", "glm", "surv")), data.step = 14, data.intro = "Change the type of analysis. Linear model calculates power with parametric assumptions, while the Generalized Linear Model and Survival Model both calculate power using a Monte Carlo approach."),
                                                             data.step = 18, data.intro = "Changing the evaluation type to a GLM Monte Carlo reveals several additional controls."),
                                                    data.step = 22, data.intro = "Survival analysis Monte Carlo power generation. This simulates data according to the design, and then censors the data if it is above or below a user defined threshold. This simulation is performed with the survreg package."),
                                           rintrojs::introBox(sliderInput(inputId = "alpha",
                                                                min = 0, max = 1, value = 0.05, label = "Alpha"), data.step = 15, data.intro = "Specify the acceptable Type-I error (false positive rate)"),
                                           shiny::conditionalPanel(
                                             condition = "input.evaltype == \'lm\' || (input.evaltype == \'glm\' && input.glmfamily == \'gaussian\') || (input.evaltype == \'surv\' && (input.distribution == \'gaussian\' || input.distribution == \'lognormal\'))",
                                             rintrojs::introBox(shiny::numericInput(inputId = "snr",
                                                                   value = 2, step = 0.1, label = "SNR"), data.step = 16, data.intro = "Signal-to-noise ratio for linear models.")
                                           ),
                                           shiny::conditionalPanel(
                                             condition = "input.evaltype == \'glm\' && input.glmfamily == \'poisson\'",
                                             shiny::fluidRow(
                                               shiny::column(width = 6,
                                                      shiny::numericInput(inputId = "poislow", "Low # of Events:",
                                                                   min = 0, value = 1)
                                               ),
                                               shiny::column(width = 6,
                                                      shiny::numericInput(inputId = "poishigh", "High # of Events:",
                                                                   min = 0, value = 2)
                                               )
                                             )
                                           ),
                                           shiny::conditionalPanel(
                                             condition = "(input.evaltype == \'glm\' && input.glmfamily == \'exponential\') || (input.evaltype == \'surv\' && input.distribution == \'exponential\')",
                                             shiny::fluidRow(
                                               shiny::column(width = 6,
                                                      shiny::numericInput(inputId = "explow", "Low Mean:",
                                                                   min = 0, value = 1)
                                               ),
                                               shiny::column(width = 6,
                                                      shiny::numericInput(inputId = "exphigh", "High Mean:",
                                                                   min = 0, value = 2)
                                               )
                                             )
                                           ),
                                           shiny::conditionalPanel(
                                             condition = "input.evaltype == \'glm\' && input.glmfamily == \'binomial\'",
                                             sliderInput(inputId = "binomialprobs", "Binomial Probabilities:",
                                                         min = 0, max = 1, value = c(0.4, 0.6))
                                           ),
                                           rintrojs::introBox(shiny::conditionalPanel(
                                             condition = "input.evaltype == \'lm\'",
                                             checkboxInput(inputId = "conservative",
                                                           label = "Conservative Power",
                                                           value = FALSE)
                                           ), data.step = 17, data.intro = "Calculates conservative effect power for 3+ level categorical factors. Calculates power once, and then sets the anticipated coefficient corresponding to the highest power level in each factor to zero. The effect power for those factors then show the most conservative power estimate."),
                                           shiny::conditionalPanel(
                                             condition = "input.evaltype == \'glm\'",
                                             rintrojs::introBox(shiny::numericInput(inputId = "nsim",
                                                                   value = 1000,
                                                                   label = "Number of Simulations"), data.step = 19, data.intro = "The number of Monte Carlo simulations to run. More simulations will result in a more precise power estimation."),
                                             rintrojs::introBox(shiny::selectInput(inputId = "glmfamily",
                                                                  choices = c("gaussian", "binomial", "poisson", "exponential"),
                                                                  label = "GLM Family"), data.step = 20, data.intro = "The distributional family used in the generalized linear model. If binomial, an additional slider will appear allowing you to change the desired upper and lower probability bounds. This automatically calculates the anticipated coefficients that correspond to that probability range."),
                                             rintrojs::introBox(checkboxInput(inputId = "parallel_eval_glm",
                                                                    label = "Parallel Evaluation (disabled on server)",
                                                                    value = FALSE), data.step = 21, data.intro = "Turn on multicore support for evaluation. Should only be used if the calculation is taking >10s to complete. Otherwise, the overhead in setting up the parallel computation outweighs the speed gains.")
                                           ),
                                           shiny::conditionalPanel(
                                             condition = "input.evaltype == \'surv\'",
                                             shiny::numericInput(inputId = "nsim_surv",
                                                          value = 1000,
                                                          label = "Number of Simulations"),
                                             shiny::selectInput(inputId = "distribution",
                                                         choices = c("gaussian", "lognormal", "exponential"),
                                                         label = "Distribution"),
                                             rintrojs::introBox(shiny::numericInput(inputId = "censorpoint",
                                                                   value = NA,
                                                                   label = "Censor Point"), data.step = 23, data.intro = "The value after (if right censored) or before (if left censored) data will be censored. The default is no censoring."),
                                             rintrojs::introBox(shiny::selectInput(inputId = "censortype",
                                                                  choices = c("right", "left"),
                                                                  label = "Censoring Type"), data.step = 24, data.intro = "The type of censoring."),
                                             checkboxInput(inputId = "parallel_eval_surv",
                                                           label = "Parallel (disabled on server)",
                                                           value = FALSE)
                                           ),
                                           checkboxInput(inputId = "colorblind",
                                                         label = "Colorblind Palette",
                                                         value = FALSE)
                                  )
                                )
                   ),
                   mainPanel(shiny::fluidRow(
                     shiny::column(width = 4, shiny::h1("Results")),
                     shiny::column(width = 2),
                     shiny::column(width = 2, rintrojs::introBox(bookmarkButton(label = "Save State", title = "Generates a URL that encodes the current state of the application for easy sharing and saving of analyses. Paste this URL into a browser (possible changing the port and address if locally different) to restore the state of the application. Be sure to set a random seed before bookmarking to recover the same results."), class = "bookmark", data.step = 33, data.intro = "Generates a URL that encodes the current state of the application for easy sharing and saving of analyses. Paste this URL into a browser (possible changing the port and address if locally different) to restore the state of the application. Be sure to set a random seed before bookmarking to recover the same results.")),
                     shiny::column(width = 2, shiny::actionButton(inputId = "tutorial", "Tutorial", icon = icon("question-circle"))),
                     shiny::column(width = 2, shiny::HTML(paste0("<div style='float:right; margin-top: 25px;'><img src=",b64,"></img></div>"))),
                     shiny::tags$style(type = "text/css", "#tutorial {margin-top: 25px;} .bookmark {margin-top: 25px;}")
                   ),
                   shiny::tabsetPanel(
                     shiny::tabPanel("Design",
                              checkboxInput(inputId = "orderdesign", label = "Order Design", value = FALSE),
                              rintrojs::introBox(tableOutput(outputId = "runmatrix"), data.step = 25, data.intro = "The generated optimal design. If hard-to-change factors are present, there will be an additional blocking column specifying the block number. Here, we have generated a design with three factors and 12 runs."),
                              shiny::hr()
                     ),
                     shiny::tabPanel("Design Evaluation",
                              rintrojs::introBox(shiny::fluidRow(
                                shiny::column(width = 12,
                                       shiny::h2("Power Results"),
                                       shiny::conditionalPanel(
                                         condition = "input.evaltype == \'lm\'",
                                         gt::gt_output(outputId = "powerresults")
                                       ),
                                       rintrojs::introBox(shiny::conditionalPanel(
                                         condition = "input.evaltype != \'lm\'",
                                         gt::gt_output(outputId = "powerresultsglm")
                                       ), data.step = 27, data.intro = "The power of the design. Output is a tidy data frame of the power and the type of evaluation for each parameter. If the evaluation type is parametric and there are 3+ level categorical factors, effect power will also be shown. Here, we have our GLM simulated power estimation.")
                                )
                              ), data.step = 26, data.intro = "This page shows the calculated/simulated power, as well as other design diagnostics. (results may take a second to appear)"),
                              shiny::hr(),
                              shiny::fluidRow(align = "center",
                                       shiny::column(width = 6,
                                              shiny::h3("Correlation Map"),
                                              rintrojs::introBox(shiny::conditionalPanel("input.numberfactors > 1",
                                                                        shiny::plotOutput(outputId = "aliasplot")), data.step = 28, data.intro = "Correlation map of the design. This shows the correlation structure between main effects and their interactions. Ideal correlation structures will be diagonal (top left to bottom right). Alias-optimal designs minimize the elements of this matrix that correspond to a main effects term interacting with an interaction term."),
                                              shiny::conditionalPanel("input.numberfactors == 1",
                                                               shiny::br(),
                                                               shiny::br(),
                                                               shiny::br(),
                                                               shiny::br(),
                                                               shiny::br(),
                                                               shiny::br(),
                                                               shiny::br(),
                                                               shiny::br(),
                                                               shiny::HTML("<font color=#898989> One Parameter: <br>No Correlation Map</font>"))
                                       ),
                                       shiny::column(width = 6,
                                              shiny::h3("Fraction of Design Space"),
                                              rintrojs::introBox(shiny::plotOutput(outputId = "fdsplot"), data.step = 29, data.intro = "Fraction of design space plot. The horizontal line corresponds to the average prediction variance for the design.")
                                       )
                              ),
                              shiny::conditionalPanel(
                                condition = "input.evaltype != \'lm\'",
                                shiny::fluidRow(
                                  shiny::hr(),
                                  shiny::column(width = 12,
                                         shiny::h3("Simulated Response Estimates"),
                                         rintrojs::introBox(shiny::plotOutput(outputId = "responsehistogram"), data.step = 30, data.intro = "Distribution of response estimates for Monte Carlo simulations. For a given design and distributional family, this plot shows the model's estimates of the overall response of the experiment (red) with the actual values on top (blue). ")
                                  )
                                )
                              ),
                              shiny::conditionalPanel(
                                condition = "input.evaltype == \'glm\'",
                                shiny::conditionalPanel(
                                  condition = "input.glmfamily != \'binomial\'",
                                  shiny::column(width = 6,
                                         shiny::numericInput(inputId = "estimatesxminglm",
                                                      value = NA,
                                                      label = "x-min")
                                  ),
                                  shiny::column(width = 6,
                                         shiny::numericInput(inputId = "estimatesxmaxglm",
                                                      value = NA,
                                                      label = "x-max")
                                  )
                                )
                              ),
                              shiny::conditionalPanel(
                                condition = "input.evaltype == \'surv\'",
                                shiny::fluidRow(
                                  shiny::column(width = 6,
                                         shiny::numericInput(inputId = "estimatesxminsurv",
                                                      value = NA,
                                                      label = "x-min")
                                  ),
                                  shiny::column(width = 6,
                                         shiny::numericInput(inputId = "estimatesxmaxsurv",
                                                      value = NA,
                                                      label = "x-max")
                                  )
                                )
                              ),
                              shiny::conditionalPanel(
                                condition = "input.evaltype != \'lm\'",
                                shiny::fluidRow(
                                  shiny::hr(),
                                  shiny::column(width = 12,
                                         shiny::h3("Simulated Estimates"),
                                         rintrojs::introBox(shiny::plotOutput(outputId = "parameterestimates"), data.step = 31, data.intro = "Individual parameter estimates for each of the design factors. The 95% confidence intervals are extracted from the actual simulated values.")
                                  )
                                )
                              ),
                              shiny::conditionalPanel(
                                condition = "input.advanceddiagnostics",
                                shiny::hr(),
                                shiny::fluidRow(align = "left",
                                         shiny::column(width = 6,
                                                shiny::conditionalPanel(
                                                  condition = "input.blockdepth1 == 'etc' && input.blockdepth2 == 'etc' && input.blockdepth3 == 'etc' && input.blockdepth4 == 'etc' && input.blockdepth5 == 'etc' && input.blockdepth6 == 'etc'",
                                                  shiny::h3("Criteria"),
                                                  shiny::h4("D"),
                                                  textOutput(outputId = "dopt"),
                                                  shiny::h4("A"),
                                                  textOutput(outputId = "aopt")
                                                ),
                                                shiny::h4("I (Average prediction variance)"),
                                                textOutput(outputId = "iopt"),
                                                shiny::conditionalPanel(
                                                  condition = "input.blockdepth1 == 'etc' && input.blockdepth2 == 'etc' && input.blockdepth3 == 'etc' && input.blockdepth4 == 'etc' && input.blockdepth5 == 'etc' && input.blockdepth6 == 'etc'",
                                                  shiny::h4("E"),
                                                  textOutput(outputId = "eopt"),
                                                  shiny::h4("G"),
                                                  textOutput(outputId = "gopt"),
                                                  shiny::h4("T"),
                                                  textOutput(outputId = "topt")
                                                )
                                         ),
                                         shiny::column(width = 6,
                                                shiny::h3("Optimal Search Values"),
                                                shiny::plotOutput(outputId = "optimalsearch")
                                         ),
                                         shiny::hr(),
                                         shiny::fluidRow(
                                           shiny::conditionalPanel(
                                             condition = "input.evaltype != \'lm\'",
                                             shiny::column(width = 12,
                                                    shiny::h3("Simulated P-Values"),
                                                    shiny::plotOutput(outputId = "simulatedpvalues")
                                             )
                                           )
                                         )
                                )
                              )
                     ),
                     shiny::tabPanel("Generating Code",
                              rintrojs::introBox(htmlOutput(outputId = "code"), data.step = 32, data.intro = "The R code used to generate the design and evaluate power. This section is updated in real time as the user changes the inputs. Copy and paste this code at the end to easily save, distribute, and reproduce your results. This also provides an easy code template to automate more complex design searches not built in to the GUI. Also included is the code showing how to analyze the experiment once the data has been collected, for all supported types of analyses. ")
                     )
                   )
                   )
                   )
                 )
  }
  plan(multisession)
  server = function(input, output, session) {
    shinyjs::disable("parallel")
    shinyjs::disable("parallel_eval_glm")
    shinyjs::disable("parallel_eval_surv")
    unique_file_name = paste0("progress_", as.character(floor(runif(1) * 1e6)), "_")
    unique_file_name2 = paste0("runmat_", as.character(floor(runif(1) * 1e6)))
    unique_file_name3 = paste0("power_", as.character(floor(runif(1) * 1e6)))
    tempfilename = tempfile(pattern = unique_file_name)
    tempdir_runmatrix = tempdir()

    progress_bar_updater = function(number1, tempfilename2 = tempfilename) {
      if (file.exists(tempfilename2)) {
        number = file.info(tempfilename2)$size - 1
        if (number < 0) {
          number = 0
        }
        close( file( tempfilename2, open="w" ) )
        } else {
        number = 0
      }
      stringtowrite = as.character(paste0(rep(0, number + 1), collapse = ""))
      prog_file = file(tempfilename2, "wb")
      writeBin(stringtowrite, prog_file)
      close(prog_file)
    }

    prog_env = new.env()
    assign("prog_first", FALSE, envir = prog_env)

    observe({
      invalidateLater(500, session)
      runmatresolved = tryCatch({resolved(shiny::isolate(runmatrix_future()))},
                                error = function(e) FALSE)
      powerresolved = tryCatch({resolved(shiny::isolate(powerresultsglm_future()))},
                               error = function(e) FALSE)
      if (!runmatresolved && exists("progress", envir = prog_env) && get("whichfuture", envir = prog_env) == "gen") {
        if (prog_env$prog_first) {
          assign("prog_first", FALSE, envir = prog_env)
          prog_env$progress$set(message = "Calculating...", value = 0)
        }
        assign("percentdone", (file.info(tempfilename)$size - 1) / shiny::isolate(input$repeats), envir = prog_env)
        progress$inc(percentdone - previouspercent)
        assign("previouspercent", percentdone, envir = prog_env)
      }
      if (!powerresolved && exists("progress", envir = prog_env) && get("whichfuture", envir = prog_env) == "pow") {
        if (prog_env$prog_first) {
          assign("prog_first", FALSE, envir = prog_env)
          prog_env$progress$set(message = "Calculating...", value = 0)
        }
        if(evaluationtype() != "surv") {
          if (shiny::isolate(input$nsim) > 50) {
            sims = 50
          } else {
            sims = shiny::isolate(input$nsim)
          }
        } else {
          if (shiny::isolate(input$nsim_surv) > 50) {
            sims = 50
          } else {
            sims = shiny::isolate(input$nsim_surv)
          }
        }

        assign("percentdone", (file.info(tempfilename)$size - 1) / sims, envir = prog_env)
        progress$inc(percentdone - previouspercent)
        assign("previouspercent", percentdone, envir = prog_env)
      }
      if (runmatresolved && exists("progress", envir = prog_env) && get("whichfuture", envir = prog_env) == "gen") {
        shinyjs::enable("submitbutton")
        shinyjs::enable("evalbutton")
        if(file.exists(tempfilename)) {
          file.remove(tempfilename)
        }
        progress$close()
        rm(progress, envir = prog_env)
      }
      if (powerresolved && exists("progress", envir = prog_env) && get("whichfuture", envir = prog_env) == "pow") {
        shinyjs::enable("submitbutton")
        shinyjs::enable("evalbutton")
        if(file.exists(tempfilename)) {
          file.remove(tempfilename)
        }
        progress$close()
        rm(progress, envir = prog_env)
      }
      if (file.exists(file.path(tempdir_runmatrix, paste0(unique_file_name2, ".Rds"))) && resolved(shiny::isolate(runmatrix_future()))) {
        tempval = tryCatch({
          readRDS(file.path(tempdir_runmatrix, paste0(unique_file_name2, ".Rds")))
        }, error = function(e) "", warning = function(w) "")
        if (inherits(tempval, "data.frame")) {
          runmatvalues$runmatrix = tempval
          tryCatch({
            file.remove(file.path(tempdir_runmatrix, paste0(unique_file_name2, ".Rds")))
          }, error = function(e) "", warning = function(w) "")
        }
      }
      if (file.exists(file.path(tempdir_runmatrix, paste0(unique_file_name3, ".Rds"))) && shiny::isolate(input$evaltype) != "lm" && powerresolved && !file.exists(tempfilename)) {
        tryCatch({
          tempvalpower = readRDS(file.path(tempdir_runmatrix, paste0(unique_file_name3, ".Rds")))
          powerresolved = FALSE
        }, error = function(e) "", warning = function(w) "")
        if (inherits(tempvalpower,"data.frame")) {
          runmatvalues$power = tempvalpower
          tryCatch({
            file.remove(file.path(tempdir_runmatrix, paste0(unique_file_name3, ".Rds")))
          }, error = function(e) "", warning = function(w) "")
        }
      }
    }, env = prog_env)

    runmatvalues = shiny::reactiveValues()

    #initialize
    # runmatvalues$runmatrix = gen_design(data.frame(X1 = c(1, -1)), ~X1, 12)
    # runmatvalues$power = eval_design(gen_design(data.frame(X1 = c(1, -1)), ~X1, 12), ~., 0.05)

    inputlist_htc = shiny::reactive({
      input$submitbutton
      inputlist1 = list()
      for (i in 1:6) {
        if ( (i == 1 && input$numberfactors > 0) && shiny::isolate(input$blockdepth1) == "htc") {
          if (input$factortype1 == "numeric") {
            inputlist1[[input$factorname1]] = seq(input$numericlow1, input$numerichigh1, length.out = input$numericlength1)
          }
          if (input$factortype1 == "discnum") {
            inputlist1[[input$factorname1]] = as.numeric(strsplit(gsub(" ", "", input$disclevels1, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype1 == "cat") {
            inputlist1[[input$factorname1]] = strsplit(gsub(" ", "", input$levels1, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 2 && input$numberfactors > 1) && shiny::isolate(input$blockdepth2) == "htc") {
          if (input$factortype2 == "numeric") {
            inputlist1[[input$factorname2]] = seq(input$numericlow2, input$numerichigh2, length.out = input$numericlength2)
          }
          if (input$factortype2 == "discnum") {
            inputlist1[[input$factorname2]] = as.numeric(strsplit(gsub(" ", "", input$disclevels2, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype2 == "cat") {
            inputlist1[[input$factorname2]] = strsplit(gsub(" ", "", input$levels2, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 3 && input$numberfactors > 2) && shiny::isolate(input$blockdepth3) == "htc") {
          if (input$factortype3 == "numeric") {
            inputlist1[[input$factorname3]] = seq(input$numericlow3, input$numerichigh3, length.out = input$numericlength3)
          }
          if (input$factortype3 == "discnum") {
            inputlist1[[input$factorname3]] = as.numeric(strsplit(gsub(" ", "", input$disclevels3, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype3 == "cat") {
            inputlist1[[input$factorname3]] = strsplit(gsub(" ", "", input$levels3, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 4 && input$numberfactors > 3) && shiny::isolate(input$blockdepth4) == "htc") {
          if (input$factortype4 == "numeric") {
            inputlist1[[input$factorname4]] = seq(input$numericlow4, input$numerichigh4, length.out = input$numericlength4)
          }
          if (input$factortype4 == "discnum") {
            inputlist1[[input$factorname4]] = as.numeric(strsplit(gsub(" ", "", input$disclevels4, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype4 == "cat") {
            inputlist1[[input$factorname4]] = strsplit(gsub(" ", "", input$levels4, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 5 && input$numberfactors > 4) && shiny::isolate(input$blockdepth5) == "htc") {
          if (input$factortype5 == "numeric") {
            inputlist1[[input$factorname5]] = seq(input$numericlow5, input$numerichigh5, length.out = input$numericlength5)
          }
          if (input$factortype5 == "discnum") {
            inputlist1[[input$factorname5]] = as.numeric(strsplit(gsub(" ", "", input$disclevels5, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype5 == "cat") {
            inputlist1[[input$factorname5]] = strsplit(gsub(" ", "", input$levels5, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 6 && input$numberfactors > 5) && shiny::isolate(input$blockdepth6) == "htc") {
          if (input$factortype6 == "numeric") {
            inputlist1[[input$factorname6]] = seq(input$numericlow6, input$numerichigh6, length.out = input$numericlength6)
          }
          if (input$factortype6 == "discnum") {
            inputlist1[[input$factorname6]] = as.numeric(strsplit(gsub(" ", "", input$disclevels6, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype6 == "cat") {
            inputlist1[[input$factorname6]] = strsplit(gsub(" ", "", input$levels6, fixed = TRUE), split = ",")[[1]]
          }
        }
      }
      inputlist1
    })

    inputlist_htctext = shiny::reactive({
      inputlist1 = list()
      for (i in 1:6) {
        if ( (i == 1 && input$numberfactors > 0) && input$blockdepth1 == "htc") {
          if (input$factortype1 == "numeric") {
            inputlist1[[input$factorname1]] = seq(input$numericlow1, input$numerichigh1, length.out = input$numericlength1)
          }
          if (input$factortype1 == "discnum") {
            inputlist1[[input$factorname1]] = as.numeric(strsplit(gsub(" ", "", input$disclevels1, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype1 == "cat") {
            inputlist1[[input$factorname1]] = strsplit(gsub(" ", "", input$levels1, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 2 && input$numberfactors > 1) && input$blockdepth2 == "htc") {
          if (input$factortype2 == "numeric") {
            inputlist1[[input$factorname2]] = seq(input$numericlow2, input$numerichigh2, length.out = input$numericlength2)
          }
          if (input$factortype2 == "discnum") {
            inputlist1[[input$factorname2]] = as.numeric(strsplit(gsub(" ", "", input$disclevels2, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype2 == "cat") {
            inputlist1[[input$factorname2]] = strsplit(gsub(" ", "", input$levels2, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 3 && input$numberfactors > 2) && input$blockdepth3 == "htc") {
          if (input$factortype3 == "numeric") {
            inputlist1[[input$factorname3]] = seq(input$numericlow3, input$numerichigh3, length.out = input$numericlength3)
          }
          if (input$factortype3 == "discnum") {
            inputlist1[[input$factorname3]] = as.numeric(strsplit(gsub(" ", "", input$disclevels3, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype3 == "cat") {
            inputlist1[[input$factorname3]] = strsplit(gsub(" ", "", input$levels3, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 4 && input$numberfactors > 3) && input$blockdepth4 == "htc") {
          if (input$factortype4 == "numeric") {
            inputlist1[[input$factorname4]] = seq(input$numericlow4, input$numerichigh4, length.out = input$numericlength4)
          }
          if (input$factortype4 == "discnum") {
            inputlist1[[input$factorname4]] = as.numeric(strsplit(gsub(" ", "", input$disclevels4, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype4 == "cat") {
            inputlist1[[input$factorname4]] = strsplit(gsub(" ", "", input$levels4, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 5 && input$numberfactors > 4) && input$blockdepth5 == "htc") {
          if (input$factortype5 == "numeric") {
            inputlist1[[input$factorname5]] = seq(input$numericlow5, input$numerichigh5, length.out = input$numericlength5)
          }
          if (input$factortype5 == "discnum") {
            inputlist1[[input$factorname5]] = as.numeric(strsplit(gsub(" ", "", input$disclevels5, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype5 == "cat") {
            inputlist1[[input$factorname5]] = strsplit(gsub(" ", "", input$levels5, fixed = TRUE), split = ",")[[1]]
          }
        }
        if ( (i == 6 && input$numberfactors > 5) && input$blockdepth6 == "htc") {
          if (input$factortype6 == "numeric") {
            inputlist1[[input$factorname6]] = seq(input$numericlow6, input$numerichigh6, length.out = input$numericlength6)
          }
          if (input$factortype6 == "discnum") {
            inputlist1[[input$factorname6]] = as.numeric(strsplit(gsub(" ", "", input$disclevels6, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype6 == "cat") {
            inputlist1[[input$factorname6]] = strsplit(gsub(" ", "", input$levels6, fixed = TRUE), split = ",")[[1]]
          }
        }
      }
      inputlist1
    })


    inputlist = shiny::reactive({
      inputlist1 = list()
      for (i in 1:6) {
        if (i == 1 && input$numberfactors > 0 ) {
          if (input$blockdepth1 == "etc") {
            if (input$factortype1 == "numeric") {
              inputlist1[[input$factorname1]] = seq(input$numericlow1, input$numerichigh1, length.out = input$numericlength1)
            }
            if (input$factortype1 == "discnum") {
              inputlist1[[input$factorname1]] = as.numeric(strsplit(gsub(" ", "", input$disclevels1, fixed = TRUE), split = ",")[[1]])
            }
            if (input$factortype1 == "cat") {
              inputlist1[[input$factorname1]] = strsplit(gsub(" ", "", input$levels1, fixed = TRUE), split = ",")[[1]]
            }
          }
        }
        if (i == 2 && input$numberfactors > 1) {
          if (input$blockdepth2 == "etc") {
            if (input$factortype2 == "numeric") {
              inputlist1[[input$factorname2]] = seq(input$numericlow2, input$numerichigh2, length.out = input$numericlength2)
            }
            if (input$factortype2 == "discnum") {
              inputlist1[[input$factorname2]] = as.numeric(strsplit(gsub(" ", "", input$disclevels2, fixed = TRUE), split = ",")[[1]])
            }
            if (input$factortype2 == "cat") {
              inputlist1[[input$factorname2]] = strsplit(gsub(" ", "", input$levels2, fixed = TRUE), split = ",")[[1]]
            }
          }
        }
        if (i == 3 && input$numberfactors > 2) {
          if (input$blockdepth3 == "etc") {
            if (input$factortype3 == "numeric") {
              inputlist1[[input$factorname3]] = seq(input$numericlow3, input$numerichigh3, length.out = input$numericlength3)
            }
            if (input$factortype3 == "discnum") {
              inputlist1[[input$factorname3]] = as.numeric(strsplit(gsub(" ", "", input$disclevels3, fixed = TRUE), split = ",")[[1]])
            }
            if (input$factortype3 == "cat") {
              inputlist1[[input$factorname3]] = strsplit(gsub(" ", "", input$levels3, fixed = TRUE), split = ",")[[1]]
            }
          }
        }
        if (i == 4 && input$numberfactors > 3) {
          if (input$blockdepth4 == "etc") {
            if (input$factortype4 == "numeric") {
              inputlist1[[input$factorname4]] = seq(input$numericlow4, input$numerichigh4, length.out = input$numericlength4)
            }
            if (input$factortype4 == "discnum") {
              inputlist1[[input$factorname4]] = as.numeric(strsplit(gsub(" ", "", input$disclevels4, fixed = TRUE), split = ",")[[1]])
            }
            if (input$factortype4 == "cat") {
              inputlist1[[input$factorname4]] = strsplit(gsub(" ", "", input$levels4, fixed = TRUE), split = ",")[[1]]
            }
          }
        }
        if (i == 5 && input$numberfactors > 4) {
          if (input$blockdepth5 == "etc") {
            if (input$factortype5 == "numeric") {
              inputlist1[[input$factorname5]] = seq(input$numericlow5, input$numerichigh5, length.out = input$numericlength5)
            }
            if (input$factortype5 == "discnum") {
              inputlist1[[input$factorname5]] = as.numeric(strsplit(gsub(" ", "", input$disclevels5, fixed = TRUE), split = ",")[[1]])
            }
            if (input$factortype5 == "cat") {
              inputlist1[[input$factorname5]] = strsplit(gsub(" ", "", input$levels5, fixed = TRUE), split = ",")[[1]]
            }
          }
        }
        if (i == 6 && input$numberfactors > 5) {
          if (input$blockdepth6 == "etc") {
            if (input$factortype6 == "numeric") {
              inputlist1[[input$factorname6]] = seq(input$numericlow6, input$numerichigh6, length.out = input$numericlength6)
            }
            if (input$factortype6 == "discnum") {
              inputlist1[[input$factorname6]] = as.numeric(strsplit(gsub(" ", "", input$disclevels6, fixed = TRUE), split = ",")[[1]])
            }
            if (input$factortype6 == "cat") {
              inputlist1[[input$factorname6]] = strsplit(gsub(" ", "", input$levels6, fixed = TRUE), split = ",")[[1]]
            }
          }
        }
      }
      inputlist1
    })

    candidatesetall = shiny::reactive({
      candidateset1 = list()
      for (i in 1:6) {
        if (i == 1 && input$numberfactors > 0 ) {
          if (input$factortype1 == "numeric") {
            candidateset1[[input$factorname1]] = seq(input$numericlow1, input$numerichigh1, length.out = input$numericlength1)
          }
          if (input$factortype1 == "discnum") {
            candidateset1[[input$factorname1]] = as.numeric(strsplit(gsub(" ", "", input$disclevels1, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype1 == "cat") {
            candidateset1[[input$factorname1]] = strsplit(gsub(" ", "", input$levels1, fixed = TRUE), split = ",")[[1]]
          }
        }
        if (i == 2 && input$numberfactors > 1) {
          if (input$factortype2 == "numeric") {
            candidateset1[[input$factorname2]] = seq(input$numericlow2, input$numerichigh2, length.out = input$numericlength2)
          }
          if (input$factortype2 == "discnum") {
            candidateset1[[input$factorname2]] = as.numeric(strsplit(gsub(" ", "", input$disclevels2, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype2 == "cat") {
            candidateset1[[input$factorname2]] = strsplit(gsub(" ", "", input$levels2, fixed = TRUE), split = ",")[[1]]
          }
        }
        if (i == 3 && input$numberfactors > 2) {
          if (input$factortype3 == "numeric") {
            candidateset1[[input$factorname3]] = seq(input$numericlow3, input$numerichigh3, length.out = input$numericlength3)
          }
          if (input$factortype3 == "discnum") {
            candidateset1[[input$factorname3]] = as.numeric(strsplit(gsub(" ", "", input$disclevels3, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype3 == "cat") {
            candidateset1[[input$factorname3]] = strsplit(gsub(" ", "", input$levels3, fixed = TRUE), split = ",")[[1]]
          }
        }
        if (i == 4 && input$numberfactors > 3) {
          if (input$factortype4 == "numeric") {
            candidateset1[[input$factorname4]] = seq(input$numericlow4, input$numerichigh4, length.out = input$numericlength4)
          }
          if (input$factortype4 == "discnum") {
            candidateset1[[input$factorname4]] = as.numeric(strsplit(gsub(" ", "", input$disclevels4, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype4 == "cat") {
            candidateset1[[input$factorname4]] = strsplit(gsub(" ", "", input$levels4, fixed = TRUE), split = ",")[[1]]
          }
        }
        if (i == 5 && input$numberfactors > 4) {
          if (input$factortype5 == "numeric") {
            candidateset1[[input$factorname5]] = seq(input$numericlow5, input$numerichigh5, length.out = input$numericlength5)
          }
          if (input$factortype5 == "discnum") {
            candidateset1[[input$factorname5]] = as.numeric(strsplit(gsub(" ", "", input$disclevels5, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype5 == "cat") {
            candidateset1[[input$factorname5]] = strsplit(gsub(" ", "", input$levels5, fixed = TRUE), split = ",")[[1]]
          }
        }
        if (i == 6 && input$numberfactors > 5) {
          if (input$factortype6 == "numeric") {
            candidateset1[[input$factorname6]] = seq(input$numericlow6, input$numerichigh6, length.out = input$numericlength6)
          }
          if (input$factortype6 == "discnum") {
            candidateset1[[input$factorname6]] = as.numeric(strsplit(gsub(" ", "", input$disclevels6, fixed = TRUE), split = ",")[[1]])
          }
          if (input$factortype6 == "cat") {
            candidateset1[[input$factorname6]] = strsplit(gsub(" ", "", input$levels6, fixed = TRUE), split = ",")[[1]]
          }
        }
      }
      candidateset1
    })



    inputstring = shiny::reactive({
      updatevector = c(input$blockdepth1, input$blockdepth2, input$blockdepth3, input$blockdepth4, input$blockdepth5, input$blockdepth6)
      commacount = input$numberfactors - 1
      finalstring = c()
      for (i in 1:6) {
        if (i == 1 && input$numberfactors > 0) {
          finalstring = c(finalstring, input$factorname1, " = ")
          if (input$factortype1 == "numeric") {
            finalstring = c(finalstring, "seq(", input$numericlow1, ",", input$numerichigh1, ", length.out = ", input$numericlength1, ")")
          }
          if (input$factortype1 == "discnum") {
            finalstring = c(finalstring, "c(", gsub(" ", "", input$disclevels1, fixed = TRUE), ")")
          }
          if (input$factortype1 == "cat") {
            levelstring = paste0(c("\""), strsplit(gsub(" ", "", input$levels1, fixed = TRUE), split = ",")[[1]], c("\","), collapse = "")
            levelstring = substr(levelstring, 1, nchar(levelstring) - 1)
            finalstring = c(finalstring, "c(", levelstring, ")")
          }
          if (commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring, paste0(c(", \n", rep("&nbsp;", 27)), collapse = ""))
          }
        }
        if ( (i == 2 && input$numberfactors > 1)) {
          finalstring = c(finalstring, input$factorname2, " = ")
          if (input$factortype2 == "numeric") {
            finalstring = c(finalstring, "seq(", input$numericlow2, ",", input$numerichigh2, ", length.out = ", input$numericlength2, ")")
          }
          if (input$factortype2 == "discnum") {
            finalstring = c(finalstring, "c(", gsub(" ", "", input$disclevels2, fixed = TRUE), ")")
          }
          if (input$factortype2 == "cat") {
            levelstring = paste0(c("\""), strsplit(gsub(" ", "", input$levels2, fixed = TRUE), split = ",")[[1]], c("\","), collapse = "")
            levelstring = substr(levelstring, 1, nchar(levelstring) - 1)
            finalstring = c(finalstring, "c(", levelstring, ")")
          }
          if (commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring, paste0(c(", \n", rep("&nbsp;", 27)), collapse = ""))
          }
        }
        if (i == 3 && input$numberfactors > 2) {
          finalstring = c(finalstring, input$factorname3, " = ")
          if (input$factortype3 == "numeric") {
            finalstring = c(finalstring, "seq(", input$numericlow3, ",", input$numerichigh3, ", length.out = ", input$numericlength3, ")")
          }
          if (input$factortype3 == "discnum") {
            finalstring = c(finalstring, "c(", gsub(" ", "", input$disclevels3, fixed = TRUE), ")")
          }
          if (input$factortype3 == "cat") {
            levelstring = paste0(c("\""), strsplit(gsub(" ", "", input$levels3, fixed = TRUE), split = ",")[[1]], c("\","), collapse = "")
            levelstring = substr(levelstring, 1, nchar(levelstring) - 1)
            finalstring = c(finalstring, "c(", levelstring, ")")
          }
          if (commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring, paste0(c(", \n", rep("&nbsp;", 27)), collapse = ""))
          }
        }
        if (i == 4 && input$numberfactors > 3) {
          finalstring = c(finalstring, input$factorname4, " = ")
          if (input$factortype4 == "numeric") {
            finalstring = c(finalstring, "seq(", input$numericlow4, ",", input$numerichigh4, ", length.out = ", input$numericlength4, ")")
          }
          if (input$factortype4 == "discnum") {
            finalstring = c(finalstring, "c(", gsub(" ", "", input$disclevels4, fixed = TRUE), ")")
          }
          if (input$factortype4 == "cat") {
            levelstring = paste0(c("\""), strsplit(gsub(" ", "", input$levels4, fixed = TRUE), split = ", ")[[1]], c("\", "), collapse = "")
            levelstring = substr(levelstring, 1, nchar(levelstring) - 1)
            finalstring = c(finalstring, "c(", levelstring, ")")
          }
          if (commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring, paste0(c(", \n", rep("&nbsp;", 27)), collapse = ""))
          }
        }
        if (i == 5 && input$numberfactors > 4) {
          finalstring = c(finalstring, input$factorname5, " = ")
          if (input$factortype5 == "numeric") {
            finalstring = c(finalstring, "seq(", input$numericlow5, ",", input$numerichigh5, ", length.out = ", input$numericlength5, ")")
          }
          if (input$factortype5 == "discnum") {
            finalstring = c(finalstring, "c(", gsub(" ", "", input$disclevels5, fixed = TRUE), ")")
          }
          if (input$factortype5 == "cat") {
            levelstring = paste0(c("\""), strsplit(gsub(" ", "", input$levels5, fixed = TRUE), split = ", ")[[1]], c("\", "), collapse = "")
            levelstring = substr(levelstring, 1, nchar(levelstring) - 1)
            finalstring = c(finalstring, "c(", levelstring, ")")
          }
          if (commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring, paste0(c(", \n", rep("&nbsp;", 27)), collapse = ""))
          }
        }
        if (i == 6 && input$numberfactors > 5) {
          finalstring = c(finalstring, input$factorname6, " = ")
          if (input$factortype6 == "numeric") {
            finalstring = c(finalstring, "seq(", input$numericlow6, ",", input$numerichigh6, ", length.out = ", input$numericlength6, ")")
          }
          if (input$factortype6 == "discnum") {
            finalstring = c(finalstring, "c(", gsub(" ", "", input$disclevels6, fixed = TRUE), ")")
          }
          if (input$factortype6 == "cat") {
            levelstring = paste0(c("\""), strsplit(gsub(" ", "", input$levels6, fixed = TRUE), split = ", ")[[1]], c("\", "), collapse = "")
            levelstring = substr(levelstring, 1, nchar(levelstring) - 1)
            finalstring = c(finalstring, "c(", levelstring, ")")
          }
        }
      }
      finalstring
    })

    regularmodelstring = shiny::reactive({
      tryCatch({
        if (any(unlist(strsplit(as.character(as.formula(input$model)[2]), "\\s\\+\\s|\\s\\*\\s|\\:")) == ".")) {
          dotreplace = paste0("(", paste0(names(candidatesetall()), collapse = " + "), ")")
          additionterms = unlist(strsplit(as.character(as.formula(input$model)[2]), "\\s\\+\\s"))
          multiplyterms = unlist(lapply(lapply(strsplit(additionterms, split = "\\s\\*\\s"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = " * "))
          interactionterms = unlist(lapply(lapply(strsplit(multiplyterms, split = "\\:"), gsub, pattern = "^\\.$", replacement = dotreplace), paste0, collapse = ":"))
          stringmodel = paste0("~", paste(interactionterms, collapse = " + "), sep = "")
        }
        paste0(as.character(as.formula(stringmodel)), collapse = "")
      }, error = function(e) {paste0(input$model, collapse = "")}
      )
    })

    modelwithblocks = shiny::reactive({
      if (isblockingtext()) {
        basemodel = gsub(pattern = "~", replacement = "", x = regularmodelstring(), fixed = TRUE)
        blockingmodelterms = "~ (1|Block1) + "
        paste0(blockingmodelterms, basemodel)
      }
    })

    contraststring = shiny::reactive({
      factorcat = c(input$factortype1, input$factortype2, input$factortype3, input$factortype4, input$factortype5, input$factortype6) == "cat"
      namecat = c(input$factorname1, input$factorname2, input$factorname3, input$factorname4, input$factorname5, input$factorname6)[factorcat]
      contrasttemp = "list("
      for (i in 1:length(namecat)) {
        if (i != length(namecat)) {
          contrasttemp = paste0(contrasttemp, namecat[i], " = ", "contr.sum, ")
        } else {
          contrasttemp = paste0(contrasttemp, namecat[i], " = ", "contr.sum)")
        }
      }
      contrasttemp
    })

    anyfactors = shiny::reactive({
      any(c(input$factortype1, input$factortype2, input$factortype3, input$factortype4, input$factortype5, input$factortype6) == "cat")
    })

    code = shiny::reactive({
      blocking = any("htc" %in% c(input$blockdepth1, input$blockdepth2, input$blockdepth3, input$blockdepth4, input$blockdepth5, input$blockdepth6)[1:input$numberfactors])

      first = paste0(c("<br><pre>",
                       "<code style=\"color:#468449\"># This is the R code used to generate these results in skpr.</code><br>",
                       "<code style=\"color:#468449\"># Copy this into an R script and rerun to reproduce these results.</code><br><br>",
                       "library(skpr)<br><br>",
                       ifelse(input$setseed,
                              paste0("<code style=\"color:#468449\">#Setting random number generator seed:</code><br>",
                                     "set.seed(", input$seed, ")<br><br>"), "<code style=\"color:#468449\">#Consider setting a seed to make this script fully reproducible.<br>#Go to Advanced->Set Random Number Generator Seed, click <br>#the checkbox, and set Random Seed to any whole number.</code><br><br>"),
                       "<code style=\"color:#468449\"># Generating candidate set:</code><br>",
                       "candidateset = expand.grid(", inputstring(), ")<br><br>",
                       ifelse(blocking,
                              paste0(c("<code style=\"color:#468449\"># Generating design for hard-to-change factors:</code> <br>",
                                       "design_htc = gen_design(candidateset = candidateset, <br>", rep("&nbsp;", 24),
                                       "model = ", blockmodel(), ", <br>", rep("&nbsp;", 24),
                                       "trials = ", as.character(input$numberblocks), ")<br><br>"), collapse = ""), ""),
                       "<code style=\"color:#468449\"># Generating design:</code><br>",
                       "design = gen_design(candidateset = candidateset, <br>", rep("&nbsp;", 20),
                       "model = ", regularmodelstring(), ", <br>", rep("&nbsp;", 20),
                       "trials = ", as.character(input$trials)
      ), collapse = "")
      if (blocking) {
        first = paste(c(first, ", <br>", rep("&nbsp;", 20),
                        "splitplotdesign = design_htc"), collapse = "")
      }
      if (input$optimality != "D") {
        first = paste(c(first, ", <br>", rep("&nbsp;", 20),
                        "optimality = \"", input$optimality, "\""), collapse = "")
      }
      if (input$repeats != 20) {
        first = paste(c(first, ", <br>", rep("&nbsp;", 20),
                        "repeats = ", input$repeats), collapse = "")
      }
      if (input$varianceratio != 1) {
        first = paste(c(first, ", <br>", rep("&nbsp;", 20),
                        "varianceratio = ", input$varianceratio), collapse = "")
      }
      if (input$aliaspower != 2) {
        first = paste(c(first, ", <br>", rep("&nbsp;", 20),
                        "aliaspower = ", input$aliaspower), collapse = "")
      }
      if (input$mindopt != 0.8) {
        first = paste(c(first, ", <br>", rep("&nbsp;", 20),
                        "minDopt = ", input$mindopt), collapse = "")
      }
      if (as.logical(input$parallel)) {
        first = paste(c(first, ", <br>", rep("&nbsp;", 20),
                        "parallel = TRUE"), collapse = "")
      }
      if (isblockingtext()) {
        first = paste(c(first, ", <br>", rep("&nbsp;", 20),
                        "add_blocking_columns = ", ifelse(input$splitanalyzable, "TRUE", "FALSE")), collapse = "")
      }
      first = paste0(c(first, ")<br><br>"), collapse = "")
      if (input$evaltype == "lm") {
        first = paste0(c(first,
                         "<code style=\"color:#468449\"># Evaluating Design:</code><br>",
                         "eval_design(design = design, <br>", rep("&nbsp;", 12),
                         "model = ", regularmodelstring(), ", <br>", rep("&nbsp;", 12),
                         "alpha = ", input$alpha), collapse = "")
        if (isblockingtext()) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 12),
                          "blocking = TRUE"), collapse = "")
        }
        if (input$snr != 2) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 12),
                          "effectsize = ", input$snr), collapse = "")
        }
        if (input$varianceratio != 1) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 12),
                          "varianceratios = ", input$varianceratio), collapse = "")
        }
        if (as.logical(input$conservative)) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 12),
                          "conservative = TRUE"), collapse = "")
        }
        if (as.logical(input$detailedoutput)) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 12),
                          "detailedoutput = TRUE"), collapse = "")
        }
        first = paste0(c(first, ")<br><br>"), collapse = "")
        first = paste0(first,
                         "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
                         "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
                         "<code style=\"color:#468449\">## First, assign the results to a column in the data frame. Each </code><br>",
                         "<code style=\"color:#468449\">## entry in the vector corresponds to the result from that run in the design. <br><br></code>",
                         "<code style=\"color:#468449\">#design$Y = results <br><br></code>",
                         ifelse(!isblockingtext(),
                                "<code style=\"color:#468449\">## Now analyze the linear model with lm:</code><br><br>",
                                "<code style=\"color:#468449\">## Now analyze the blocked linear model with lmer (from the lme4 package) and lmerTest:<br><br></code>"),
                         ifelse(!isblockingtext(),
                                paste0("<code style=\"color:#468449\">#lm(formula = Y ", regularmodelstring(),
                                       ", data = design",
                                       ifelse(anyfactors(),
                                              paste0(", </code><br><code style=\"color:#468449\">#   ", "contrasts = ", contraststring(), ")</code>"),
                                              ")<br><br></code>")),
                                paste0(ifelse(input$splitanalyzable, "", "<code style=\"color:#468449\">## Note: Argument add_blocking_columns needs to be active in last gen_design call in order<br>## to analyze data taking into account the split-plot structure. The code below assumes that is true. <br><br></code>"),
                                  "<code style=\"color:#468449\">#library(lmerTest)<br>#lme4::lmer(formula = Y ",
                                  modelwithblocks(),
                                  ", data = design",
                                  ifelse(anyfactors(), paste0(", <br>#          ", "contrasts = ", contraststring(), "))<br><br>"), "))<br><br></code>"))))
      }
      if (input$evaltype == "glm") {
        first = paste0(c(first,
                         "<code style=\"color:#468449\"># Evaluating (Monte Carlo) Design:</code><br>",
                         "eval_design_mc(design = design, <br>", rep("&nbsp;", 15),
                         "model = ", regularmodelstring(), ", <br>", rep("&nbsp;", 15),
                         "alpha = ", input$alpha), collapse = "")
        if (isblockingtext()) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 15),
                          "blocking = TRUE"), collapse = "")
        }
        if (input$nsim != 1000) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 15),
                          "nsim = ", input$nsim), collapse = "")
        }
        if (input$glmfamily != "gaussian") {
          first = paste(c(first, ", <br>", rep("&nbsp;", 15),
                          "glmfamily = \"", input$glmfamily, "\""), collapse = "")
        }
        if (length(effectsize()) == 1) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 15),
                          "effectsize = ", effectsize()), collapse = "")
        } else {
          first = paste(c(first, ", <br>", rep("&nbsp;", 15),
                          "effectsize = c(", effectsize()[1], ", ", effectsize()[2], ")"), collapse = "")
        }
        if (!is.null(input$varianceratios)) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 15),
                          "varianceratios = ", input$varianceratio), collapse = "")
        }
        if (as.logical(input$parallel_eval_glm)) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 15),
                          "parallel = TRUE"), collapse = "")
        }
        if (as.logical(input$detailedoutput)) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 15),
                          "detailedoutput = TRUE"), collapse = "")
        }
        first = paste0(c(first, ")<br><br>"), collapse = "")
        first = paste0(first,
                       "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
                       "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
                       "<code style=\"color:#468449\">## First, assign the results to a column in the data frame. Each </code><br>",
                       "<code style=\"color:#468449\">## entry in the vector corresponds to the result from that run in the design. <br><br></code>",
                       "<code style=\"color:#468449\">#design$Y = results <br><br></code>",
                       ifelse(!isblockingtext(),
                              "<code style=\"color:#468449\">## Now analyze the generalized linear model with glm:</code><br><br>",
                              "<code style=\"color:#468449\">## Now analyze the blocked generalized linear model with glmer (from the lme4 package):<br><br></code>"),
                       ifelse(!isblockingtext(),
                              paste0("<code style=\"color:#468449\">#glm(formula = Y ", regularmodelstring(),
                                     ", data = design",
                                     ", <br>#   family = ", ifelse(input$glmfamily == "exponential", "Gamma(link=\"log\")", paste0("\"", input$glmfamily, "\"")),
                                     ifelse(anyfactors(),
                                            paste0(", </code><br><code style=\"color:#468449\">#   ", "contrasts = ", contraststring(), ")</code>"),
                                            ")<br><br></code>")),
                              paste0(ifelse(input$splitanalyzable, "", "<code style=\"color:#468449\">## Note: Argument add_blocking_columns needs to be active in last gen_design call in order<br>## to analyze data taking into account the split-plot structure. The code below assumes that is true. <br><br></code>"),
                                     "<code style=\"color:#468449\">#lme4::glmer(formula = Y ",
                                     modelwithblocks(),
                                     ", data = design",
                                     ", <br>#          family = ", ifelse(input$glmfamily == "exponential", "Gamma(link=\"log\")", paste0("\"", input$glmfamily, "\"")),
                                     ifelse(anyfactors(), paste0(", <br>#          ", "contrasts = ", contraststring(), ")"), ")</code>"))))
      }
      if (input$evaltype == "surv") {
        first = paste0(c(first,
                         "<code style=\"color:#468449\"># Evaluating (Monte Carlo Survival) Design:</code><br>",
                         "eval_design_survival_mc(design = design, <br>", rep("&nbsp;", 24),
                         "model = ", as.character(as.formula(input$model)), ", <br>", rep("&nbsp;", 24),
                         "alpha = ", input$alpha), collapse = "")
        if (input$nsim_surv != 1000) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 24),
                          "nsim = ", input$nsim_surv), collapse = "")
        }
        if (input$distribution != "gaussian") {
          first = paste(c(first, ", <br>", rep("&nbsp;", 24),
                          "distribution = \"", input$distribution, "\""), collapse = "")
        }
        if (length(effectsize()) == 1) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 24),
                          "effectsize = ", effectsize()), collapse = "")
        } else {
          first = paste(c(first, ", <br>", rep("&nbsp;", 24),
                          "effectsize = c(", effectsize()[1], ", ", effectsize()[2], ")"), collapse = "")
        }
        if (!is.na(input$censorpoint)) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 24),
                          "censorpoint = ", input$censorpoint), collapse = "")
        }
        if (input$censortype != "right") {
          first = paste(c(first, ", <br>", rep("&nbsp;", 24),
                          "censortype = \"", input$censortype, "\""), collapse = "")
        }
        if (as.logical(input$parallel_eval_surv)) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 24),
                          "parallel = TRUE"), collapse = "")
        }
        if (as.logical(input$detailedoutput)) {
          first = paste(c(first, ", <br>", rep("&nbsp;", 24),
                          "detailedoutput = TRUE"), collapse = "")
        }
        first = paste0(c(first, ")<br><br>"), collapse = "")
        first = paste0(first,
                       "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
                       "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
                       "<code style=\"color:#468449\">## This is a survival model, so first create a Surv object that marks the censored runs.</code><br>",
                       "<code style=\"color:#468449\">## Each entry in the results vector corresponds to the result from that run in the design.</code><br>",
                       "<code style=\"color:#468449\">## Here, the raw censored responses are given as NA. We replace those values with the</code><br>",
                       "<code style=\"color:#468449\">## censor point and use the event argument to mark them as censored. </code><br>",
                       "<code style=\"color:#468449\">## (Surv argument \"event\" is TRUE when \"results\" is not censored, and FALSE when censored).<br><br></code>",
                       "<code style=\"color:#468449\">#notcensored = !is.na(results) </code><br>",
                       ifelse(!is.na(input$censorpoint), paste0("<code style=\"color:#468449\">#results[is.na(results)] = ", input$censorpoint, "</code><br>"),
                              ""),
                       "<code style=\"color:#468449\">#design$Y = Surv(time=results, event = notcensored, type = \"", input$censortype, "\") <br><br></code>",
                       "<code style=\"color:#468449\">## Now analyze the linear model with survreg (from the survival package):</code><br><br>",
                        paste0("<code style=\"color:#468449\">#survival::survreg(formula = Y ", regularmodelstring(),
                                     ", data = design, dist = \"", input$distribution, "\")"))
      }

      first = paste0(c(first, "</pre></code>"), collapse = "")
      first
    })

    isblocking = shiny::reactive({
      input$submitbutton
      any("htc" %in% c(shiny::isolate(input$blockdepth1),
                       shiny::isolate(input$blockdepth2),
                       shiny::isolate(input$blockdepth3),
                       shiny::isolate(input$blockdepth4),
                       shiny::isolate(input$blockdepth5),
                       shiny::isolate(input$blockdepth6))[1:shiny::isolate(input$numberfactors)])
    })
    isblockingtext = shiny::reactive({
      any("htc" %in% c(input$blockdepth1,
                       input$blockdepth2,
                       input$blockdepth3,
                       input$blockdepth4,
                       input$blockdepth5,
                       input$blockdepth6)[1:(input$numberfactors)])
    })

    blockmodel = shiny::reactive({
      if (input$model == "~.") {
        as.formula(paste0("~", paste(names(inputlist_htctext()), collapse = " + ")))
      } else {
        names = names(inputlist())
        modelsplit = attr(terms.formula(as.formula(input$model), data = candidatesetall()), "term.labels")
        regularmodel = rep(FALSE, length(modelsplit))
        for (term in names) {
          regex = paste0("(\\b", term, "\\b)|(\\b", term, ":)|(:", term, "\\b)|(\\b", term, "\\s\\*)|(\\*\\s", term, "\\b)|(:", term, ":)")
          regularmodel = regularmodel | grepl(regex, modelsplit, perl = TRUE)
        }
        paste0("~", paste(modelsplit[!regularmodel], collapse = " + "))

      }
    })

    optimality = shiny::reactive({
      input$submitbutton
      if (shiny::isolate(input$numberfactors) == 1 && shiny::isolate(input$optimality) == "Alias") {
        shiny::showNotification("Alias-optimal design selected with only one factor: Switching to D-optimal.", type = "warning", duration = 10)
        shiny::updateSelectInput(session, "optimality", choices = c("D", "I", "A", "Alias", "G", "E", "T"), selected = "D")
        "D"
      } else {
        shiny::isolate(input$optimality)
      }
    })

    effectsize = shiny::reactive({
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
          return(c(input$poislow, input$poishigh))
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

    runmatrix_future = eventReactive(input$submitbutton, {
      shinyjs::disable("submitbutton")
      shinyjs::disable("evalbutton")
      assign("previouspercent", 0, envir = prog_env)
      assign("percentdone", 0, envir = prog_env)
      assign("prog_first", TRUE, envir = prog_env)
      assign("progress", shiny::Progress$new(), envir = prog_env)
      assign("whichfuture", "gen", envir = prog_env)
      prog_env$progress$set(message = "Setting up...", value = 0)
      if (input$setseed) {
        set.seed(input$seed)
      }
      if (!isblocking()) {
        optimality_async = optimality()
        candidatesetall_async = expand.grid(candidatesetall())
        model_async = input$model
        trials_async = input$trials
        repeats_async = input$repeats
        aliaspower_async = input$aliaspower
        mindopt_async =  input$mindopt
        return(future({
          temp = skpr::gen_design(candidateset = candidatesetall_async,
                           model = c("~", substring(model_async, 2)),
                           trials = trials_async,
                           optimality = optimality_async,
                           repeats = repeats_async,
                           aliaspower = aliaspower_async,
                           minDopt = mindopt_async,
                           advancedoptions = list(GUI = TRUE, progressBarUpdater = progress_bar_updater))
          attr(temp, "generating.model") = NULL
          saveRDS(temp, file = file.path(tempdir_runmatrix, paste0(unique_file_name2, ".Rds")))
        }))
      } else {
        spd = gen_design(candidateset = expand.grid(candidatesetall()),
                         model = as.formula(blockmodel()),
                         trials = input$numberblocks,
                         optimality = ifelse(toupper(optimality()) == "ALIAS" && length(inputlist_htc()) == 1, "D", optimality()),
                         repeats = input$repeats,
                         varianceratio = input$varianceratio,
                         aliaspower = input$aliaspower,
                         minDopt = input$mindopt)
        optimality_async = optimality()
        candidatesetall_async = expand.grid(candidatesetall())
        model_async = input$model
        trials_async = input$trials
        repeats_async = input$repeats
        aliaspower_async = input$aliaspower
        varianceratio_async = input$varianceratio
        mindopt_async =  input$mindopt
        add_blocking_columns_aync = input$splitanalyzable
        return(future({
          temp = skpr::gen_design(candidateset = candidatesetall_async,
                   model = as.formula(model_async),
                   trials = trials_async,
                   splitplotdesign = spd,
                   optimality = optimality_async,
                   repeats = repeats_async,
                   varianceratio = varianceratio_async,
                   aliaspower = aliaspower_async,
                   minDopt = mindopt_async,
                   add_blocking_columns = add_blocking_columns_aync,
                   advancedoptions = list(GUI = TRUE, progressBarUpdater = progress_bar_updater))
          attr(temp, "generating.model") = NULL
          saveRDS(temp, file = file.path(tempdir_runmatrix, paste0(unique_file_name2, ".Rds")))
        }))
      }
    })

    evaluationtype = eventReactive(input$evalbutton, {
      shiny::isolate(input$evaltype)
    })

    pal_option = function(n) {
      if(input$colorchoice == "A") {
        viridis::magma(n)
      } else if(input$colorchoice == "B") {
        viridis::inferno(n)
      } else if(input$colorchoice == "C") {
        viridis::plasma(n)
      } else if(input$colorchoice == "D") {
        viridis::viridis(n)
      } else {
        "white"
      }
    }

    style_matrix = function(runmat, order_vals = FALSE, alpha = 0.3, trials, optimality) {
      . = NULL
      if(order_vals) {
        new_runmat = runmat[do.call(order, runmat),, drop=FALSE ]
        rownames(new_runmat) = 1:nrow(new_runmat)

        display_rm = gt::gt(new_runmat[do.call(order, new_runmat),, drop=FALSE ],
                        rownames_to_stub = TRUE) %>%
          gt::tab_stubhead("Run") %>%
          gt::tab_options(data_row.padding = gt::px(10))  %>%
          gt::tab_spanner(
            label = "Factors",
            columns = colnames(.)
          ) %>% gt::tab_header(
            title = "Design",
            subtitle = sprintf("%d-run %s-optimal design",
                               trials,
                               optimality)
          )
      } else {
        display_rm = gt::gt(runmat,rownames_to_stub = TRUE) %>%
          gt::tab_stubhead("Run") %>%
          gt::tab_options(data_row.padding = gt::px(10)) %>%
          gt::tab_spanner(
            label = "Factors",
            columns = colnames(.)
          ) %>% gt::tab_header(
            title = "Design",
            subtitle = sprintf("%d-run %s-optimal design",
                               trials,
                               optimality)
          )
      }
      cols_rm = colnames(runmat)
      for(i in seq_len(length(cols_rm))) {
        if(is.numeric(runmat[,i])) {
          display_rm = display_rm %>%
            gt::data_color(
              columns = cols_rm[i],
              palette = pal_option(100),
              alpha = alpha,
              autocolor_text = FALSE)
        } else {
          display_rm = display_rm %>%
            gt::data_color(
              columns = cols_rm[i],
              palette = pal_option(length(unique(runmat[,i]))),
              alpha = alpha,
              autocolor_text = FALSE)
        }
      }
      display_rm
    }


    format_table = function(powerval, display_table, alpha, nsim, colorblind) {
      color_bad = "red"
      color_maybe = "yellow"
      if(colorblind) {
        color_bad = "purple"
        color_maybe = "orange"
      }
      display_table = display_table %>%
        gt::data_color(columns = "power",
                   palette = scales::col_numeric(palette =colorRampPalette(c("white", "darkgreen"))(100),
                                                domain =c(0,1)),
                   alpha = 0.3,
                   autocolor_text = FALSE) %>%
        gt::tab_options(table.width = gt::pct(100))
      if(any(powerval$power <= alpha + 1/sqrt(nsim) &
             powerval$power >= alpha - 1/sqrt(nsim))) {
        display_table = display_table %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = color_maybe,alpha=0.3)
            ),
            locations = gt::cells_body(
              columns = "power",
              rows = power <= alpha + 1/sqrt(nsim))
          ) %>%
          gt::tab_source_note(
            source_note = sprintf("Note: Power values marked in %s are within the simulation uncertainty for user-specified Type-I error (increase the number of simulations)",
                                  color_maybe)
          )
      }
      if(any(powerval$power < alpha - 1/sqrt(nsim))) {
        display_table = display_table %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = color_bad,alpha=0.3)
            ),
            locations = gt::cells_body(
              columns = "power",
              rows = (power < alpha - 1/sqrt(nsim)))
          ) %>%
          gt::tab_source_note(
            source_note = sprintf("Note: Power values marked in %s fall below the user-specified Type-I error (%0.2f)",
                                  color_bad, alpha)
          )
      }
      return(display_table)
    }

    output$runmatrix = gt::render_gt({
      req(runmatvalues$runmatrix)
      style_matrix(runmatvalues$runmatrix, order_vals = input$orderdesign,  trials = shiny::isolate(input$trials), optimality = shiny::isolate(input$optimality))
    }, align = "left")

    powerresults = eventReactive({
      input$evalbutton
    }, {
      calculate_results = function(runmat) {
        if (evaluationtype() == "lm") {
          eval_design(design = runmat,
                      model = as.formula(input$model),
                      alpha = input$alpha,
                      blocking = isblocking(),
                      effectsize = effectsize(),
                      conservative = input$conservative,
                      detailedoutput = input$detailedoutput)
        }
      }
      calculate_results(runmatvalues$runmatrix)
    })
    powerresultsglm_future = eventReactive({
      input$evalbutton
    }, {
      if (evaluationtype() == "glm") {
        shinyjs::disable("submitbutton")
        shinyjs::disable("evalbutton")
        assign("previouspercent", 0, envir = prog_env)
        assign("percentdone", 0, envir = prog_env)
        assign("prog_first", TRUE, envir = prog_env)
        assign("progress", shiny::Progress$new(), envir = prog_env)
        assign("whichfuture", "pow", envir = prog_env)
        prog_env$progress$set(message = "Setting up...", value = 0)
        if (input$setseed) {
          set.seed(input$seed)
        }
        runmat_async = runmatvalues$runmatrix
        model_async = as.formula(input$model)
        alpha_async = input$alpha
        blocking_async = isblocking()
        nsim_async = input$nsim
        varianceratios_async = input$varianceratio
        glmfamily_async = input$glmfamily
        effectsize_async = effectsize()
        detailedoutput_async = input$detailedoutput
        return(future({
          temp = skpr::eval_design_mc(design = runmat_async,
                                model = model_async,
                                alpha = alpha_async,
                                blocking = blocking_async,
                                nsim = nsim_async,
                                varianceratios = varianceratios_async,
                                glmfamily = glmfamily_async,
                                effectsize = effectsize_async,
                                detailedoutput = detailedoutput_async,
                                advancedoptions = list(GUI = TRUE, progressBarUpdater = progress_bar_updater))
          attr(temp, "generating.model") = NULL
          saveRDS(temp, file = file.path(tempdir_runmatrix, paste0(unique_file_name3, ".Rds")))
        }))
      }
      if (evaluationtype() == "surv") {
        shinyjs::disable("submitbutton")
        shinyjs::disable("evalbutton")
        assign("previouspercent", 0, envir = prog_env)
        assign("percentdone", 0, envir = prog_env)
        assign("prog_first", TRUE, envir = prog_env)
        assign("progress", shiny::Progress$new(), envir = prog_env)
        assign("whichfuture", "pow", envir = prog_env)
        prog_env$progress$set(message = "Setting up...", value = 0)
        if (input$setseed) {
          set.seed(input$seed)
        }
        if (isblocking()) {
          print("Hard-to-change factors are not supported for survival designs. Evaluating design with no blocking.")
        }
        runmat_async = runmatvalues$runmatrix
        model_async = as.formula(input$model)
        alpha_async = input$alpha
        nsim_async = input$nsim_surv
        censorpoint_async = input$censorpoint
        censortype_async = input$censortype
        distribution_async = input$distribution
        effectsize_async = effectsize()
        detailedoutput_async = input$detailedoutput
        return(future({
          temp = eval_design_survival_mc(design = runmat_async,
                                model = model_async,
                                alpha = alpha_async,
                                nsim = nsim_async,
                                censorpoint = censorpoint_async,
                                censortype = censortype_async,
                                distribution = distribution_async,
                                effectsize = effectsize_async,
                                detailedoutput = detailedoutput_async,
                                advancedoptions = list(GUI = TRUE, progressBarUpdater = progress_bar_updater))
          attr(temp, "generating.model") = NULL
          saveRDS(temp, file = file.path(tempdir_runmatrix, paste0(unique_file_name3, ".Rds")))
          })
        )
      }
    })


    output$powerresults = gt::render_gt({
      display_table = gt::gt(powerresults())
      format_table(powerresults(),display_table, shiny::isolate(input$alpha),shiny::isolate(input$nsim),shiny::isolate(input$colorblind))
    }, align = "left")

    output$powerresultsglm = gt::render_gt({
      req(runmatvalues$power)
      display_table = gt::gt(runmatvalues$power)
      if(evaluationtype() != "surv") {
        return(format_table(runmatvalues$power,display_table, shiny::isolate(input$alpha),shiny::isolate(input$nsim),shiny::isolate(input$colorblind)))
      } else {
        return(format_table(runmatvalues$power,display_table, shiny::isolate(input$alpha),shiny::isolate(input$nsim_surv),shiny::isolate(input$colorblind)))
      }
    }, align = "left")

    output$aliasplot = renderPlot({
      tryCatch({
        plot_correlations(runmatvalues$runmatrix)
      }, error = function(e) {
      })
    })

    output$fdsplot = renderPlot({
      req(runmatvalues$runmatrix)
      format_fdsplot = function(runmat) {
        plot_fds(runmat, model = as.formula(shiny::isolate(input$model)))
      }
      format_fdsplot(runmatvalues$runmatrix)
    })

    output$code = renderUI({
      shiny::HTML(code())
    })

    output$dopt = renderText({
      req(runmatvalues$runmatrix)
      attr(runmatvalues$runmatrix, "D")
    })
    output$aopt = renderText({
      req(runmatvalues$runmatrix)
      attr(runmatvalues$runmatrix, "A")
    })
    output$iopt = renderText({
      req(runmatvalues$runmatrix)
      attr(runmatvalues$runmatrix, "I")
    })
    output$eopt = renderText({
      req(runmatvalues$runmatrix)
      attr(runmatvalues$runmatrix, "E")
    })
    output$gopt = renderText({
      req(runmatvalues$runmatrix)
      attr(runmatvalues$runmatrix, "G")
    })
    output$topt = renderText({
      req(runmatvalues$runmatrix)
      attr(runmatvalues$runmatrix, "T")
    })
    output$optimalsearch = renderPlot({
      req(runmatvalues$runmatrix)
      format_search = function(runmat) {
        if (shiny::isolate(optimality()) %in% c("D", "G", "A")) {
          if(attr(runmat, "blocking") || attr(runmat, "splitplot")) {
            max_y_val = max(attr(runmat, "optimalsearchvalues"),na.rm=TRUE)
            statement = "Optimality Value (higher is better)"
          }  else {
            max_y_val = 100
            statement = "Efficiency (higher is better)"
          }
          shiny::isolate(plot(attr(runmat, "optimalsearchvalues"), xlab = "Search Iteration", ylab = paste(shiny::isolate(optimality()), statement),
                       type = "p", col = "red", pch = 16, ylim = c(0, max_y_val)))
          shiny::isolate(points(x = attr(runmat, "best"), y = attr(runmat, "optimalsearchvalues")[attr(runmat, "best")],
                         type = "p", col = "green", pch = 16, cex = 2, ylim = c(0, max_y_val)))
        } else {
          if (shiny::isolate(shiny::isolate(optimality())) == "I") {
            shiny::isolate(plot(attr(runmat, "optimalsearchvalues"), xlab = "Search Iteration", ylab = "Average Prediction Variance (lower is better)", type = "p", col = "red", pch = 16))
          } else {
            shiny::isolate(plot(attr(runmat, "optimalsearchvalues"), xlab = "Search Iteration", ylab = paste(shiny::isolate(optimality()), "Criteria Value (higher is better)"), type = "p", col = "red", pch = 16))
          }
          shiny::isolate(points(x = attr(runmat, "best"), y = attr(runmat, "optimalsearchvalues")[attr(runmat, "best")], type = "p", col = "green", pch = 16, cex = 2))
        }
      }
      format_search(runmatvalues$runmatrix)
    })
    output$simulatedpvalues = renderPlot({
      req(runmatvalues$power)
      input$evalbutton
      pvalrows = shiny::isolate(floor(ncol(attr(runmatvalues$power, "pvals")) / 3) + 1)
      if (!is.null(attr(runmatvalues$power, "pvals"))) {
        par(mfrow = c(pvalrows, 3))
        for (col in 1:shiny::isolate(ncol(attr(runmatvalues$power, "pvals")))) {
          shiny::isolate(hist(attr(runmatvalues$power, "pvals")[, col], breaks = seq(0, 1, 0.05), main = colnames(attr(runmatvalues$power, "pvals"))[col], xlim = c(0, 1), xlab = "p values", ylab = "Count", col = "red", pch = 16))
        }
      }
    })
    output$parameterestimates = renderPlot({
      req(runmatvalues$power)

      if(shiny::isolate(evaluationtype()) == "glm") {
        if (!is.null(attr(runmatvalues$power, "estimates"))) {
          ests = apply(attr(runmatvalues$power, "estimates"), 2, quantile, c(0.05, 0.5, 0.95))
          truth = attr(runmatvalues$power, "anticoef")
          if (shiny::isolate(input$glmfamily) == "binomial") {
            ests = exp(ests) / (1 + exp(ests))
            truth = exp(truth) / (1 + exp(truth))
          }
          if (shiny::isolate(input$glmfamily) == "poisson") {
            ests = exp(ests)
            truth = exp(truth)
          }
          if (shiny::isolate(input$glmfamily) == "exponential") {
            ests = exp(ests)
            truth = exp(truth)
          }
          par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
          plot(x = 1:length(colnames(ests)), y = ests[2, ],
               xaxt = "n",
               xlab = "Parameters",
               ylab = ifelse(shiny::isolate(input$glmfamily) == "binomial", "Parameter Estimates (Probability)", "Parameter Estimates"),
               ylim = ifelse(rep(shiny::isolate(input$glmfamily) == "binomial", 2), c(0, 1), c(min(as.vector(ests)), max(as.vector(ests)))),
               xlim = c(0.5, length(colnames(ests)) + 0.5),
               type = "p", pch = 16, col = "red", cex = 1)
          axis(1, at = 1:length(colnames(ests)), labels = colnames(ests), las = 2)
          legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
          par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
          grid(nx = NA, ny = NULL)
          arrows(x0 = 1:length(colnames(ests)), y0 = ests[1, ], x1 = 1:length(colnames(ests)), y1 = ests[3, ], length = 0.05, angle = 90, code = 3)
          points(x = 1:length(colnames(ests)), y = truth, pch = 16, col = "blue", cex = 1)
          title("Simulated Parameter Estimates (5%-95% Confidence Intervals)")
        }
      }
      if (shiny::isolate(evaluationtype()) == "surv") {
        if (!is.null(attr(runmatvalues$power, "estimates"))) {
          ests = apply(attr(runmatvalues$power, "estimates"), 2, quantile, c(0.05, 0.5, 0.95))
          truth = attr(runmatvalues$power, "anticoef")
          if (shiny::isolate(input$distribution) == "exponential") {
            ests = exp(ests)
            truth = exp(truth)
          }
          par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
          plot(x = 1:length(colnames(ests)), y = ests[2, ],
               xaxt = "n",
               xlab = "Parameters",
               ylab = ifelse(shiny::isolate(input$glmfamily) == "binomial", "Parameter Estimates (Probability)", "Parameter Estimates"),
               ylim = ifelse(rep(shiny::isolate(input$glmfamily) == "binomial", 2), c(0, 1), c(min(as.vector(ests)), max(as.vector(ests)))),
               xlim = c(0.5, length(colnames(ests)) + 0.5),
               type = "p", pch = 16, col = "red", cex = 1)
          axis(1, at = 1:length(colnames(ests)), labels = colnames(ests), las = 2)
          legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
          par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
          grid(nx = NA, ny = NULL)
          arrows(x0 = 1:length(colnames(ests)), y0 = ests[1, ], x1 = 1:length(colnames(ests)), y1 = ests[3, ], length = 0.05, angle = 90, code = 3)
          points(x = 1:length(colnames(ests)), y = truth, pch = 16, col = "blue", cex = 1)
          title("Simulated Parameter Estimates (5%-95% Confidence Intervals)")
        }
      }
    })

    output$responsehistogram = renderPlot({
      req(runmatvalues$power)

      updatevector = c(runmatvalues$power)
      if(shiny::isolate(evaluationtype()) == "glm") {
        if (!is.null(attr(runmatvalues$power, "estimates"))) {
          responses = as.vector(attr(runmatvalues$power, "estimates") %*% t(attr(runmatvalues$power, "modelmatrix")))
          trueresponses = as.vector(attr(runmatvalues$power, "anticoef") %*% t(attr(runmatvalues$power, "modelmatrix")))
          widths = hist(trueresponses, plot = FALSE)$counts
          widths = widths[widths != 0]
          widths = sqrt(widths)
          uniquevalues = length(table(responses))
          breakvalues = ifelse(uniquevalues < shiny::isolate(input$nsim) * shiny::isolate(input$trials) / 10, uniquevalues, shiny::isolate(input$nsim) * shiny::isolate(input$trials) / 10)
          if (shiny::isolate(input$glmfamily) == "binomial") {
            responses = exp(responses) / (1 + exp(responses))
            trueresponses = exp(trueresponses) / (1 + exp(trueresponses))
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
            hist(responses, breaks = breakvalues, xlab = "Response (Probability)", main = "Distribution of Simulated Response Estimatese", xlim = c(0, 1))
            legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
            grid(nx = NA, ny = NULL)
            hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses Estimates", xlab = "Response", ylab = "Count", col = "red", border = "red")
            abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
          }
          if (shiny::isolate(input$glmfamily) == "poisson") {
            responses = exp(responses)
            trueresponses = exp(trueresponses)
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
            hist(responses, breaks = breakvalues, xlab = "Response", main = "Distribution of Simulated Response Estimates", xlim = c(ifelse(is.na(input$estimatesxminglm), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminglm), ifelse(is.na(input$estimatesxmaxglm), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxglm)), col = "red", border = "red")
            legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
            grid(nx = NA, ny = NULL)
            hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses ", xlab = "Response", ylab = "Count", col = "red", border = "red")
            abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
          }
          if (shiny::isolate(input$glmfamily) == "exponential") {
            responses = exp(responses)
            trueresponses = exp(trueresponses)
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
            hist(responses, breaks = breakvalues, xlab = "Response", main = "Distribution of Simulated Response Estimates", xlim = c(ifelse(is.na(input$estimatesxminglm), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminglm), ifelse(is.na(input$estimatesxmaxglm), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxglm)), col = "red", border = "red")
            legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
            grid(nx = NA, ny = NULL)
            hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses", xlab = "Response", ylab = "Count", col = "red", border = "red")
            abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
          }
          if (shiny::isolate(input$glmfamily) == "gaussian") {
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
            hist(responses, breaks = breakvalues, xlab = "Response", main = "Distribution of Simulated Response Estimates", xlim = c(ifelse(is.na(input$estimatesxminglm), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminglm), ifelse(is.na(input$estimatesxmaxglm), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxglm)), col = "red", border = "red")
            legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
            grid(nx = NA, ny = NULL)
            hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses", xlab = "Response", ylab = "Count", col = "red", border = "red")
            abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)

          }
        }
      }

      if(shiny::isolate(evaluationtype()) == "surv") {
        if (!is.null(attr(runmatvalues$power, "estimates"))) {
          responses = as.vector(attr(runmatvalues$power, "estimates") %*% t(attr(runmatvalues$power, "modelmatrix")))
          trueresponses = as.vector(attr(runmatvalues$power, "anticoef") %*% t(attr(runmatvalues$power, "modelmatrix")))
          filtered_string = ""
          if(shiny::isolate(input$distribution) == "exponential") {
            #Filter out extreme values
            mad_trueresp = 10*max(exp(trueresponses))
            num_filtered = sum(exp(responses) > mad_trueresp)
            responses = responses[exp(responses) < mad_trueresp]
            trueresponses = trueresponses[exp(trueresponses) < mad_trueresp]
            filtered_string = sprintf(" (%g extreme outliers removed)",num_filtered)
          }
          widths = hist(trueresponses, plot = FALSE)$counts
          widths = widths[widths != 0]
          widths = sqrt(widths)
          uniquevalues = length(table(responses))
          breakvalues = ifelse(uniquevalues < shiny::isolate(input$nsim) * shiny::isolate(input$trials) / 10, uniquevalues, shiny::isolate(input$nsim) * shiny::isolate(input$trials) / 10)
          if (shiny::isolate(input$distribution) == "exponential") {
            responses = exp(responses)
            trueresponses = exp(trueresponses)
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
            hist(responses, breaks = breakvalues, xlab = "Response", main = sprintf("Distribution of Simulated Response Estimates%s", filtered_string), xlim = c(ifelse(is.na(input$estimatesxminsurv), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminsurv), ifelse(is.na(input$estimatesxmaxsurv), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxsurv)), col = "red", border = "red")
            legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
            grid(nx = NA, ny = NULL)
            hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses", xlab = "Response", ylab = "Count", col = "red", border = "red")
            abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
          }
          if (shiny::isolate(input$distribution) %in% c("gaussian", "lognormal")) {
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
            hist(responses, breaks = breakvalues, xlab = "Response", main = "Distribution of Simulated Response Estimates (from survival analysis)", xlim = c(ifelse(is.na(input$estimatesxminsurv), min(hist(responses, plot = FALSE)$breaks), input$estimatesxminsurv), ifelse(is.na(input$estimatesxmaxsurv), max(hist(responses, plot = FALSE)$breaks), input$estimatesxmaxsurv)), col = "red", border = "red")
            legend("topright", inset = c(-0.2, 0), legend = c("Truth", "Simulated"), pch = c(16, 16), col = c("blue", "red"), title = "Estimates")
            par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = FALSE)
            grid(nx = NA, ny = NULL)
            hist(responses, breaks = breakvalues, add = TRUE, main = "Distribution of Simulated Responses", xlab = "Response", ylab = "Count", col = "red", border = "red")
            abline(v = unique(trueresponses)[order(unique(trueresponses))], col = adjustcolor("blue", alpha.f = 0.40), lwd = widths)
          }
        }
      }
    })

    output$separationwarning = renderText({
      req(runmatvalues$power)

      input$evalbutton
      likelyseparation = FALSE
      if (shiny::isolate(input$evaltype) == "glm" && shiny::isolate(input$glmfamily) == "binomial") {
        if (!is.null(attr(runmatvalues$power, "pvals"))) {
          pvalmat = attr(runmatvalues$power, "pvals")
          for (i in 2:ncol(pvalmat)) {
            pvalcount = hist(pvalmat[, i], breaks = seq(0, 1, 0.05), plot = FALSE)
            likelyseparation = likelyseparation || (all(pvalcount$count[20] > pvalcount$count[17:19]) && pvalcount$count[20] > shiny::isolate(input$nsim) / 15)
          }
        }
      }
      if (likelyseparation) {
        shiny::showNotification("Partial or complete separation likely detected in the binomial Monte Carlo simulation. Increase the number of runs in the design or decrease the number of model parameters to improve power.", type = "warning", duration = 10)
      }
    })
    shiny::observeEvent(input$tutorial,
                 rintrojs::introjs(session,
                         options = list("showProgress" = "true",
                                        "showBullets" = "false"),
                         events = list(
                   "onchange" = I("
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
                  ))
                ))
    shiny::outputOptions(output, "separationwarning", suspendWhenHidden = FALSE)
  }

  shiny::runGadget(shiny::shinyApp(ui, server, enableBookmarking = "url"), viewer = shiny::dialogViewer(dialogName = "skprGUIserver", width = 1200, height = 1200))
}
# nocov end
globalVariables(c("progress", "percentdone", "previouspercent"))
