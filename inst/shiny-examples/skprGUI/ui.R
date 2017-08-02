#ui.R for skprGUI

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(h1("Inputs"),
                 fluidRow(
                   column(width=6,
                          actionButton("submitbutton", "Generate Design",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   ),
                   column(width=6,
                          actionButton("evalbutton", "Evaluate Design",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   )
                 ),
                 br(),br(),
                 tabsetPanel(
                   tabPanel(
                     "Basic",
                     numericInput(inputId = "trials",
                                  12, label = "Trials"),
                     textInput(inputId = "model",
                               "~.", label = "Model"),
                     conditionalPanel(condition = "input.blockdepth1 == 'htc' || input.blockdepth2 == 'htc' || input.blockdepth3 == 'htc' || input.blockdepth4 == 'htc' || input.blockdepth5 == 'htc' || input.blockdepth6 == 'htc'",
                                      fluidRow(
                                        column(width=12,numericInput(inputId = "numberblocks",
                                                                     4, label = "Number of blocks"))
                                      )
                     ),
                     conditionalPanel(condition = "input.numberfactors == 6",
                                      fluidRow(
                                        column(width=12,
                                               HTML("<p style=\"color: #F00;\">skprGUI only supports up to 6 factors. Alter the generated code to add more.</p>")
                                        )
                                      )
                     ),
                     numericInput(inputId = "numberfactors",
                                  min=1,max=6, 1, label = "Number of Factors"),
                     br(),
                     wellPanel(h3("Factor 1"),
                               fluidRow(
                                 column(width=5,
                                        selectInput(inputId = "blockdepth1",
                                                    choices=list("Easy"="etc","Hard" = "htc"),
                                                    label="Changes")
                                 ),
                                 column(width=7,
                                        selectInput(inputId = "factortype1",
                                                    choices=list("Continuous"="numeric","Categorical" = "cat", "Discrete Numeric"="discnum"),
                                                    label="Type")
                                 )
                               ),
                               fluidRow(
                                 column(width=12,
                                        textInput(inputId = "factorname1",
                                                  value="X1",
                                                  label="Name")
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.factortype1 == \'numeric\'",
                                 fluidRow(
                                   column(width=4,
                                          numericInput(inputId = "numericlow1",
                                                       value=-1,
                                                       label="Low")
                                   ),
                                   column(width=4,
                                          numericInput(inputId = "numerichigh1",
                                                       value=1,
                                                       label="High")
                                   ),
                                   column(width=4,
                                          numericInput(inputId = "numericlength1",
                                                       value=3,
                                                       label="Breaks")
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.factortype1 == \'discnum\'",
                                 fluidRow(
                                   column(width=12,
                                          textInput(inputId = "disclevels1",
                                                    value="",
                                                    label="Levels (separate with commas)")
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.factortype1 == \'cat\'",
                                 fluidRow(
                                   column(width=12,
                                          textInput(inputId = "levels1",
                                                    value="",
                                                    label="Levels (separate with commas)")
                                   )
                                 )
                               )
                     ),
                     conditionalPanel(
                       condition = "input.numberfactors > 1",
                       wellPanel(h3("Factor 2"),
                                 fluidRow(
                                   column(width=5,
                                          selectInput(inputId = "blockdepth2",
                                                      choices=list("Easy"="etc","Hard" = "htc"),
                                                      label="Changes")
                                   ),
                                   column(width=7,
                                          selectInput(inputId = "factortype2",
                                                      choices=list("Continuous"="numeric","Categorical" = "cat", "Discrete Numeric"="discnum"),
                                                      label="Type")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=12,
                                          textInput(inputId = "factorname2",
                                                    value="X2",
                                                    label="Name")
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype2 == \'numeric\'",
                                   fluidRow(
                                     column(width=4,
                                            numericInput(inputId = "numericlow2",
                                                         value=-1,
                                                         label="Low")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numerichigh2",
                                                         value=1,
                                                         label="High")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numericlength2",
                                                         value=3,
                                                         label="Breaks")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype2 == \'discnum\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "disclevels2",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype2 == \'cat\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "levels2",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.numberfactors > 2",
                       wellPanel(h3("Factor 3"),
                                 fluidRow(
                                   column(width=5,
                                          selectInput(inputId = "blockdepth3",
                                                      choices=list("Easy"="etc","Hard" = "htc"),
                                                      label="Changes")
                                   ),
                                   column(width=7,
                                          selectInput(inputId = "factortype3",
                                                      choices=list("Continuous"="numeric","Categorical" = "cat", "Discrete Numeric"="discnum"),
                                                      label="Type")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=12,
                                          textInput(inputId = "factorname3",
                                                    value="X3",
                                                    label="Name")
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype3 == \'numeric\'",
                                   fluidRow(
                                     column(width=4,
                                            numericInput(inputId = "numericlow3",
                                                         value=-1,
                                                         label="Low")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numerichigh3",
                                                         value=1,
                                                         label="High")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numericlength3",
                                                         value=3,
                                                         label="Breaks")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype3 == \'discnum\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "disclevels3",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype3 == \'cat\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "levels3",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.numberfactors > 3",
                       wellPanel(h3("Factor 4"),
                                 fluidRow(
                                   column(width=5,
                                          selectInput(inputId = "blockdepth4",
                                                      choices=list("Easy"="etc","Hard" = "htc"),
                                                      label="Changes")
                                   ),
                                   column(width=7,
                                          selectInput(inputId = "factortype4",
                                                      choices=list("Continuous"="numeric","Categorical" = "cat", "Discrete Numeric"="discnum"),
                                                      label="Type")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=12,
                                          textInput(inputId = "factorname4",
                                                    value="X4",
                                                    label="Name")
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype4 == \'numeric\'",
                                   fluidRow(
                                     column(width=4,
                                            numericInput(inputId = "numericlow4",
                                                         value=-1,
                                                         label="Low")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numerichigh4",
                                                         value=1,
                                                         label="High")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numericlength4",
                                                         value=3,
                                                         label="Breaks")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype4 == \'discnum\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "disclevels4",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype4 == \'cat\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "levels4",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.numberfactors > 4",
                       wellPanel(h3("Factor 5"),
                                 fluidRow(
                                   column(width=5,
                                          selectInput(inputId = "blockdepth5",
                                                      choices=list("Easy"="etc","Hard" = "htc"),
                                                      label="Changes")
                                   ),
                                   column(width=7,
                                          selectInput(inputId = "factortype5",
                                                      choices=list("Continuous"="numeric","Categorical" = "cat", "Discrete Numeric"="discnum"),
                                                      label="Type")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=12,
                                          textInput(inputId = "factorname5",
                                                    value="X5",
                                                    label="Name")
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype5 == \'numeric\'",
                                   fluidRow(
                                     column(width=4,
                                            numericInput(inputId = "numericlow5",
                                                         value=-1,
                                                         label="Low")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numerichigh5",
                                                         value=1,
                                                         label="High")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numericlength5",
                                                         value=3,
                                                         label="Breaks")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype5 == \'discnum\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "disclevels5",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype5 == \'cat\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "levels5",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.numberfactors > 5",
                       wellPanel(h3("Factor 6"),
                                 fluidRow(
                                   column(width=5,
                                          selectInput(inputId = "blockdepth6",
                                                      choices=list("Easy"="etc","Hard" = "htc"),
                                                      label="Changes")
                                   ),
                                   column(width=7,
                                          selectInput(inputId = "factortype6",
                                                      choices=list("Continuous"="numeric","Categorical" = "cat", "Discrete Numeric"="discnum"),
                                                      label="Type")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=12,
                                          textInput(inputId = "factorname6",
                                                    value="X6",
                                                    label="Name")
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype6 == \'numeric\'",
                                   fluidRow(
                                     column(width=4,
                                            numericInput(inputId = "numericlow6",
                                                         value=-1,
                                                         label="Low")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numerichigh6",
                                                         value=1,
                                                         label="High")
                                     ),
                                     column(width=4,
                                            numericInput(inputId = "numericlength6",
                                                         value=3,
                                                         label="Breaks")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype6 == \'discnum\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "disclevels6",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.factortype6 == \'cat\'",
                                   fluidRow(
                                     column(width=12,
                                            textInput(inputId = "levels6",
                                                      value="",
                                                      label="Levels (separate with commas)")
                                     )
                                   )
                                 )
                       )
                     )
                   ),
                   tabPanel("Advanced",
                            selectInput(inputId = "optimality",
                                        choices = c("D","I","A","Alias","G","E","T"),
                                        label = "Optimality"),
                            numericInput(inputId = "repeats",
                                         10, label = "Repeats"),
                            numericInput(inputId = "varianceratio",
                                         1, label = "Variance Ratio"),
                            conditionalPanel(
                              condition = "input.optimality == \'Alias\'",
                              numericInput(inputId = "aliaspower",
                                           min=2,value=2, label = "Alias Optimal Interaction Level"),
                              sliderInput(inputId = "mindopt",
                                          min=0,max=1,value=0.95, label = "Minimum D Optimality")
                            ),
                            checkboxInput(inputId = "setseed",
                                          label = "Set Random Number Generator Seed",
                                          value=FALSE),
                            conditionalPanel(
                              condition = "input.setseed",
                              numericInput(inputId = "seed",
                                           1, label = "Random Seed")
                            ),
                            checkboxInput(inputId = "parallel",
                                          label = "Parallel Evaluation",
                                          value = FALSE),
                            checkboxInput(inputId = "splitanalyzable",
                                          label = "Include Blocking Columns in Run Matrix",
                                          value=FALSE),
                            checkboxInput(inputId = "detailedoutput",
                                          label = "Detailed Output",
                                          value=FALSE),
                            checkboxInput(inputId = "advanceddiagnostics",
                                          label = "Advanced Design Diagnostics",
                                          value=FALSE)
                   ),
                   tabPanel("Power",
                            radioButtons(inputId = "evaltype",
                                         label="Model Type",
                                         choiceNames = c("Linear Model","Generalized Linear Model","Survival Model"),
                                         choiceValues = c("lm","glm","surv")),
                            sliderInput(inputId = "alpha",
                                        min=0,max=1,value=0.05, label = "Alpha"),
                            numericInput(inputId = "delta",
                                         value=2, step=0.1, label = "Delta"),
                            conditionalPanel(
                              condition = "input.evaltype == \'lm\'",
                              checkboxInput(inputId = "conservative",
                                            label = "Conservative Power",
                                            value=FALSE)
                            ),

                            conditionalPanel(
                              condition = "input.evaltype == \'glm\'",
                              numericInput(inputId = "nsim",
                                           value=1000,
                                           label = "Number of Simulations"),
                              selectInput(inputId = "glmfamily",
                                          choices = c("gaussian","binomial","poisson","exponential"),
                                          label = "GLM Family"),
                              conditionalPanel(
                                condition = "input.glmfamily == \'binomial\'",
                                sliderInput(inputId = "binomialprobs", "Binomial Probabilities:",
                                            min = 0, max = 1, value = c(0.4,0.6))
                              ),
                              checkboxInput(inputId = "parallel_eval_glm",
                                            label = "Parallel",
                                            value=FALSE)
                            ),

                            conditionalPanel(
                              condition = "input.evaltype == \'surv\'",
                              numericInput(inputId = "nsim_surv",
                                           value=1000,
                                           label = "Number of Simulations"),
                              selectInput(inputId = "distribution",
                                          choices = c("gaussian","lognormal","exponential"),
                                          label = "Distribution"),
                              numericInput(inputId = "censorpoint",
                                           value=NA,
                                           label = "Censor Point"),
                              selectInput(inputId = "censortype",
                                          choices = c("right","left"),
                                          label = "Censoring Type"),
                              checkboxInput(inputId = "parallel_eval_surv",
                                            label = "Parallel",
                                            value=FALSE)
                            )
                   )
                 )
    ),
    mainPanel(h1("Results"),
              tabsetPanel(
                tabPanel("Design",
                         h2("Design"),
                         tableOutput(outputId = "runmatrix"),
                         hr()
                ),
                tabPanel("Design Evaluation",
                         fluidRow(
                           column(width=6,
                                  h2("Power Results"),
                                  conditionalPanel(
                                    condition = "input.evaltype == \'lm\'",
                                    tableOutput(outputId = "powerresults")
                                  ),
                                  conditionalPanel(
                                    condition = "input.evaltype == \'glm\'",
                                    tableOutput(outputId = "powerresultsglm")
                                  ),
                                  conditionalPanel(
                                    condition = "input.evaltype == \'surv\'",
                                    tableOutput(outputId = "powerresultssurv")
                                  )
                           ),
                           column(width=6,
                                  conditionalPanel(align="left",
                                                   condition = "output.separationwarning != \'\'",
                                                   h2("Note:"),
                                                   htmlOutput(outputId = "separationwarning")
                                  )
                           )
                         ),
                         hr(),
                         fluidRow(align="center",
                                  column(width=6,
                                         h3("Correlation Map"),
                                         plotOutput(outputId = "aliasplot")
                                  ),
                                  column(width=6,
                                         (h3("Fraction of Design Space")),
                                         plotOutput(outputId = "fdsplot")
                                  )
                         ),
                         conditionalPanel(
                           condition = "input.evaltype == \'glm\'",
                           fluidRow(
                             hr(),
                             column(width=12,
                                    h3("Simulated Response"),
                                    plotOutput(outputId = "responsehistogram")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.evaltype == \'surv\'",
                           fluidRow(
                             hr(),
                             column(width=12,
                                    h3("Simulated Response"),
                                    plotOutput(outputId = "responsehistogramsurv")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.evaltype == \'glm\'",
                           fluidRow(
                             hr(),
                             column(width=12,
                                    h3("Simulated Estimates"),
                                    plotOutput(outputId = "parameterestimates")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.advanceddiagnostics",
                           hr(),
                           fluidRow(align="left",
                                    column(width=6,
                                           conditionalPanel(
                                             condition = "input.blockdepth1 == 'etc' && input.blockdepth2 == 'etc' && input.blockdepth3 == 'etc' && input.blockdepth4 == 'etc' && input.blockdepth5 == 'etc' && input.blockdepth6 == 'etc'",
                                             h3("Criteria"),
                                             h4("D"),
                                             textOutput(outputId = "dopt"),
                                             h4("A"),
                                             textOutput(outputId = "aopt")
                                           ),
                                           h4("I (Average prediction variance)"),
                                           textOutput(outputId = "iopt"),
                                           conditionalPanel(
                                             condition = "input.blockdepth1 == 'etc' && input.blockdepth2 == 'etc' && input.blockdepth3 == 'etc' && input.blockdepth4 == 'etc' && input.blockdepth5 == 'etc' && input.blockdepth6 == 'etc'",
                                             h4("E"),
                                             textOutput(outputId = "eopt"),
                                             h4("G"),
                                             textOutput(outputId = "gopt"),
                                             h4("T"),
                                             textOutput(outputId = "topt")
                                           )
                                    ),
                                    column(width=6,
                                           h3("Optimal Search Values"),
                                           plotOutput(outputId = "optimalsearch")
                                    ),
                                    hr(),
                                    fluidRow(
                                      conditionalPanel(
                                        condition = "input.evaltype != \'lm\'",
                                        column(width=12,
                                               h3("Simulated P-Values"),
                                               plotOutput(outputId = "simulatedpvalues")
                                        )
                                      )
                                    )
                           )
                         )
                ),
                tabPanel("Generating Code",
                         htmlOutput(outputId = "code")
                )
              )
    )
  )
)
)
