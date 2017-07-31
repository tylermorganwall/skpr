#'@title Graphical User Interface for skpr
#'
#'@description skprGUI provides a graphical user interface to skpr.
#'
#'@param inputValue1 It's the #1 input value.
#'@param inputValue2 Runner up input value, participation award pending
#'
#'@import shiny
#'@export

skprGUI = function(inputValue1,inputValue2) {

  ui = fluidPage(
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
                              radioButtons(inputId = "parallel",
                                           choiceNames = list("Single Core","Multicore"),
                                           choiceValues = list("FALSE","TRUE"),
                                           label = "Parallel"),
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

  server = function(input, output) {

    inputlist_htc = reactive({
      input$submitbutton
      inputlist1 = list()
      for(i in 1:6) {
        if((i == 1 && (input$numberfactors) > 0) && isolate(input$blockdepth1) == "htc") {
          if((input$factortype1) == "numeric") {
            inputlist1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
          }
          if((input$factortype1) == "discnum") {
            inputlist1[[(input$factorname1)]] = as.numeric(strsplit((input$disclevels1),split=",")[[1]])
          }
          if((input$factortype1) == "cat") {
            inputlist1[[(input$factorname1)]] = strsplit((input$levels1),split=",")[[1]]
          }
        }
        if((i == 2 && (input$numberfactors) > 1) && isolate(input$blockdepth2) == "htc") {
          if((input$factortype2) == "numeric") {
            inputlist1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
          }
          if((input$factortype2) == "discnum") {
            inputlist1[[(input$factorname2)]] = as.numeric(strsplit((input$disclevels2),split=",")[[1]])
          }
          if((input$factortype2) == "cat") {
            inputlist1[[(input$factorname2)]] = strsplit((input$levels2),split=",")[[1]]
          }
        }
        if((i == 3 && (input$numberfactors) > 2) && isolate(input$blockdepth3) == "htc") {
          if((input$factortype3) == "numeric") {
            inputlist1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
          }
          if((input$factortype3) == "discnum") {
            inputlist1[[(input$factorname3)]] = as.numeric(strsplit((input$disclevels3),split=",")[[1]])
          }
          if((input$factortype3) == "cat") {
            inputlist1[[(input$factorname3)]] = strsplit((input$levels3),split=",")[[1]]
          }
        }
        if((i == 4 && (input$numberfactors) > 3) && isolate(input$blockdepth4) == "htc") {
          if((input$factortype4) == "numeric") {
            inputlist1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
          }
          if((input$factortype4) == "discnum") {
            inputlist1[[(input$factorname4)]] = as.numeric(strsplit((input$disclevels4),split=",")[[1]])
          }
          if((input$factortype4) == "cat") {
            inputlist1[[(input$factorname4)]] = strsplit((input$levels4),split=",")[[1]]
          }
        }
        if((i == 5 && (input$numberfactors) > 4) && isolate(input$blockdepth5) == "htc") {
          if((input$factortype5) == "numeric") {
            inputlist1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
          }
          if((input$factortype5) == "discnum") {
            inputlist1[[(input$factorname5)]] = as.numeric(strsplit((input$disclevels5),split=",")[[1]])
          }
          if((input$factortype5) == "cat") {
            inputlist1[[(input$factorname5)]] = strsplit((input$levels5),split=",")[[1]]
          }
        }
        if((i == 6 && (input$numberfactors) > 5) && isolate(input$blockdepth6) == "htc") {
          if((input$factortype6) == "numeric") {
            inputlist1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
          }
          if((input$factortype6) == "discnum") {
            inputlist1[[(input$factorname6)]] = as.numeric(strsplit((input$disclevels6),split=",")[[1]])
          }
          if((input$factortype6) == "cat") {
            inputlist1[[(input$factorname6)]] = strsplit((input$levels6),split=",")[[1]]
          }
        }
      }
      inputlist1
    })

    inputlist_htctext = reactive({
      inputlist1 = list()
      for(i in 1:6) {
        if((i == 1 && (input$numberfactors) > 0) && (input$blockdepth1) == "htc") {
          if((input$factortype1) == "numeric") {
            inputlist1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
          }
          if((input$factortype1) == "discnum") {
            inputlist1[[(input$factorname1)]] = as.numeric(strsplit((input$disclevels1),split=",")[[1]])
          }
          if((input$factortype1) == "cat") {
            inputlist1[[(input$factorname1)]] = strsplit((input$levels1),split=",")[[1]]
          }
        }
        if((i == 2 && (input$numberfactors) > 1) && (input$blockdepth2) == "htc") {
          if((input$factortype2) == "numeric") {
            inputlist1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
          }
          if((input$factortype2) == "discnum") {
            inputlist1[[(input$factorname2)]] = as.numeric(strsplit((input$disclevels2),split=",")[[1]])
          }
          if((input$factortype2) == "cat") {
            inputlist1[[(input$factorname2)]] = strsplit((input$levels2),split=",")[[1]]
          }
        }
        if((i == 3 && (input$numberfactors) > 2) && (input$blockdepth3) == "htc") {
          if((input$factortype3) == "numeric") {
            inputlist1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
          }
          if((input$factortype3) == "discnum") {
            inputlist1[[(input$factorname3)]] = as.numeric(strsplit((input$disclevels3),split=",")[[1]])
          }
          if((input$factortype3) == "cat") {
            inputlist1[[(input$factorname3)]] = strsplit((input$levels3),split=",")[[1]]
          }
        }
        if((i == 4 && (input$numberfactors) > 3) && (input$blockdepth4) == "htc") {
          if((input$factortype4) == "numeric") {
            inputlist1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
          }
          if((input$factortype4) == "discnum") {
            inputlist1[[(input$factorname4)]] = as.numeric(strsplit((input$disclevels4),split=",")[[1]])
          }
          if((input$factortype4) == "cat") {
            inputlist1[[(input$factorname4)]] = strsplit((input$levels4),split=",")[[1]]
          }
        }
        if((i == 5 && (input$numberfactors) > 4) && (input$blockdepth5) == "htc") {
          if((input$factortype5) == "numeric") {
            inputlist1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
          }
          if((input$factortype5) == "discnum") {
            inputlist1[[(input$factorname5)]] = as.numeric(strsplit((input$disclevels5),split=",")[[1]])
          }
          if((input$factortype5) == "cat") {
            inputlist1[[(input$factorname5)]] = strsplit((input$levels5),split=",")[[1]]
          }
        }
        if((i == 6 && (input$numberfactors) > 5) && (input$blockdepth6) == "htc") {
          if((input$factortype6) == "numeric") {
            inputlist1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
          }
          if((input$factortype6) == "discnum") {
            inputlist1[[(input$factorname6)]] = as.numeric(strsplit((input$disclevels6),split=",")[[1]])
          }
          if((input$factortype6) == "cat") {
            inputlist1[[(input$factorname6)]] = strsplit((input$levels6),split=",")[[1]]
          }
        }
      }
      inputlist1
    })


    inputlist = reactive({
      input$submitbutton
      inputlist1 = list()
      for(i in 1:6) {
        if(i == 1 && (input$numberfactors) > 0 ) {
          if(input$blockdepth1 == "etc") {
            if((input$factortype1) == "numeric") {
              inputlist1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
            }
            if((input$factortype1) == "discnum") {
              inputlist1[[(input$factorname1)]] = as.numeric(strsplit((input$disclevels1),split=",")[[1]])
            }
            if((input$factortype1) == "cat") {
              inputlist1[[(input$factorname1)]] = strsplit((input$levels1),split=",")[[1]]
            }
          }
        }
        if(i == 2 && (input$numberfactors) > 1) {
          if(input$blockdepth2 == "etc") {
            if((input$factortype2) == "numeric") {
              inputlist1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
            }
            if((input$factortype2) == "discnum") {
              inputlist1[[(input$factorname2)]] = as.numeric(strsplit((input$disclevels2),split=",")[[1]])
            }
            if((input$factortype2) == "cat") {
              inputlist1[[(input$factorname2)]] = strsplit((input$levels2),split=",")[[1]]
            }
          }
        }
        if(i == 3 && (input$numberfactors) > 2) {
          if(input$blockdepth3 == "etc") {
            if((input$factortype3) == "numeric") {
              inputlist1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
            }
            if((input$factortype3) == "discnum") {
              inputlist1[[(input$factorname3)]] = as.numeric(strsplit((input$disclevels3),split=",")[[1]])
            }
            if((input$factortype3) == "cat") {
              inputlist1[[(input$factorname3)]] = strsplit((input$levels3),split=",")[[1]]
            }
          }
        }
        if(i == 4 && (input$numberfactors) > 3) {
          if(input$blockdepth4 == "etc") {
            if((input$factortype4) == "numeric") {
              inputlist1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
            }
            if((input$factortype4) == "discnum") {
              inputlist1[[(input$factorname4)]] = as.numeric(strsplit((input$disclevels4),split=",")[[1]])
            }
            if((input$factortype4) == "cat") {
              inputlist1[[(input$factorname4)]] = strsplit((input$levels4),split=",")[[1]]
            }
          }
        }
        if(i == 5 && (input$numberfactors) > 4) {
          if(input$blockdepth5 == "etc") {
            if((input$factortype5) == "numeric") {
              inputlist1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
            }
            if((input$factortype5) == "discnum") {
              inputlist1[[(input$factorname5)]] = as.numeric(strsplit((input$disclevels5),split=",")[[1]])
            }
            if((input$factortype5) == "cat") {
              inputlist1[[(input$factorname5)]] = strsplit((input$levels5),split=",")[[1]]
            }
          }
        }
        if(i == 6 && (input$numberfactors) > 5) {
          if(input$blockdepth6 == "etc") {
            if((input$factortype6) == "numeric") {
              inputlist1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
            }
            if((input$factortype6) == "discnum") {
              inputlist1[[(input$factorname6)]] = as.numeric(strsplit((input$disclevels6),split=",")[[1]])
            }
            if((input$factortype6) == "cat") {
              inputlist1[[(input$factorname6)]] = strsplit((input$levels6),split=",")[[1]]
            }
          }
        }
      }
      inputlist1
    })

    inputstring = reactive({
      updatevector = c(input$blockdepth1,input$blockdepth2,input$blockdepth3,input$blockdepth4,input$blockdepth5,input$blockdepth6)
      commacount = input$numberfactors-1
      finalstring = c()
      for(i in 1:6) {
        if(i == 1 && (input$numberfactors) > 0) {
          finalstring = c(finalstring,(input$factorname1)," = ")
          if((input$factortype1) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow1),",",(input$numerichigh1),", length.out=",input$numericlength1,")")
          }
          if((input$factortype1) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels1),")")
          }
          if((input$factortype1) == "cat") {
            len = length(strsplit(input$levels1,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels1,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
        if((i == 2 && (input$numberfactors) > 1)) {
          finalstring = c(finalstring, input$factorname2," = ")
          if((input$factortype2) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow2),",",(input$numerichigh2),", length.out=",input$numericlength2,")")
          }
          if((input$factortype2) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels2),")")
          }
          if((input$factortype2) == "cat") {
            len = length(strsplit(input$levels2,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels2,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
        if(i == 3 && (input$numberfactors) > 2) {
          finalstring = c(finalstring,input$factorname3," = ")
          if((input$factortype3) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow3),",",(input$numerichigh3),", length.out=",input$numericlength3,")")
          }
          if((input$factortype3) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels3),")")
          }
          if((input$factortype3) == "cat") {
            len = length(strsplit(input$levels3,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels3,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
        if(i == 4 && (input$numberfactors) > 3) {
          finalstring = c(finalstring,input$factorname4," = ")
          if((input$factortype4) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow4),",",(input$numerichigh4),", length.out=",input$numericlength4,")")
          }
          if((input$factortype4) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels4),")")
          }
          if((input$factortype4) == "cat") {
            len = length(strsplit(input$levels4,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels4,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
        if(i == 5 && (input$numberfactors) > 4) {
          finalstring = c(finalstring,input$factorname5," = ")
          if((input$factortype5) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow5),",",(input$numerichigh5),", length.out=",input$numericlength5,")")
          }
          if((input$factortype5) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels5),")")
          }
          if((input$factortype5) == "cat") {
            len = length(strsplit(input$levels5,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels5,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
        if(i == 6 && (input$numberfactors) > 5) {
          finalstring = c(finalstring,input$factorname6," = ")
          if((input$factortype6) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow6),",",(input$numerichigh6),", length.out=",input$numericlength6,")")
          }
          if((input$factortype6) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels6),")")
          }
          if((input$factortype6) == "cat") {
            len = length(strsplit(input$levels6,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels6,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
        }
      }
      finalstring
    })

    code = reactive({
      blocking = any("htc" %in% c(input$blockdepth1,input$blockdepth2,input$blockdepth3,input$blockdepth4,input$blockdepth5,input$blockdepth6)[1:input$numberfactors])

      first = paste0(c("<br><pre>",
                       "<code style=\"color:#468449\"># This is the R code used to generate these results in skpr.</code><br>",
                       "<code style=\"color:#468449\"># Copy this into an R script and rerun to reproduce these results.</code><br><br>",
                       ifelse(input$setseed,
                              paste0("<code style=\"color:#468449\">#Setting random number generator seed:</code><br>",
                                     "set.seed(", input$seed, ")<br><br>"),""),
                       "<code style=\"color:#468449\"># Generating candidate set:</code><br>",
                       "candidateset = expand.grid(", inputstring(), ")<br><br>",
                       ifelse(blocking,
                              paste0(c("<code style=\"color:#468449\"># Generating design for hard-to-change factors:</code> <br>",
                                       "design_htc = gen_design(candidateset = candidateset, <br>", rep("&nbsp;",24),
                                       "model = ", blockmodel(), ",<br>", rep("&nbsp;",24),
                                       "trials = ", as.character(input$numberblocks),")<br><br>"),collapse=""),""),
                       "<code style=\"color:#468449\"># Generating design:</code><br>",
                       "design = gen_design(candidateset = candidateset, <br>", rep("&nbsp;",20),
                       "model = ", as.character(as.formula(input$model)), ",<br>", rep("&nbsp;",20),
                       "trials = ", as.character(input$trials)
      ),collapse="")
      if(blocking) {
        first = paste(c(first, ",<br>", rep("&nbsp;",20),
                        "splitplotdesign = design_htc"),collapse = "")
      }
      if(input$trials %% input$numberblocks == 0) {
        sizevector = input$trials/input$numberblocks
      } else {
        sizevector = c(rep(ceiling(input$trials/input$numberblocks),input$numberblocks))
        unbalancedruns = ceiling(input$trials/input$numberblocks)*input$numberblocks - input$trials
        sizevector[(length(sizevector)-unbalancedruns+1):length(sizevector)] = sizevector[(length(sizevector)-unbalancedruns+1):length(sizevector)] -1
        sizevector = paste0(c("c(",paste0(sizevector,collapse=","),")"),collapse="")
      }
      if(isblockingtext()) {
        first = paste(c(first, ",<br>", rep("&nbsp;",20),
                        "splitplotsizes = ",sizevector),collapse = "")
      }
      if(input$optimality != "D") {
        first = paste(c(first, ",<br>", rep("&nbsp;",20),
                        "optimality = \"",input$optimality,"\""),collapse = "")
      }
      if(input$repeats != 10) {
        first = paste(c(first, ",<br>", rep("&nbsp;",20),
                        "repeats = ",input$repeats),collapse = "")
      }
      if(input$varianceratio != 1) {
        first = paste(c(first, ",<br>", rep("&nbsp;",20),
                        "varianceratio = ",input$varianceratio),collapse = "")
      }
      if(input$aliaspower != 2) {
        first = paste(c(first, ",<br>", rep("&nbsp;",20),
                        "aliaspower = ",input$aliaspower),collapse = "")
      }
      if(input$mindopt != 0.95) {
        first = paste(c(first, ",<br>", rep("&nbsp;",20),
                        "minDopt = ",input$mindopt),collapse = "")
      }
      if(as.logical(input$parallel)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",20),
                        "parallel = TRUE"),collapse = "")
      }
      first = paste0(c(first,")<br><br>"),collapse="")
      if(input$evaltype == "lm") {
        first = paste0(c(first,
                         "<code style=\"color:#468449\"># Evaluating Design:</code><br>",
                         "eval_design(RunMatrix = design,<br>", rep("&nbsp;",12),
                         "model = ", as.character(as.formula(input$model)), ",<br>", rep("&nbsp;",12),
                         "alpha = ", input$alpha),collapse="")
        if(isblockingtext()) {
          first = paste(c(first, ",<br>", rep("&nbsp;",12),
                          "blocking = TRUE"),collapse = "")
        }
        if(input$delta != 2) {
          first = paste(c(first, ",<br>", rep("&nbsp;",12),
                          "delta = ",input$delta),collapse = "")
        }
        if(input$varianceratio != 1) {
          first = paste(c(first, ",<br>", rep("&nbsp;",12),
                          "varianceratios = ",input$varianceratio),collapse = "")
        }
        if(as.logical(input$conservative)) {
          first = paste(c(first, ",<br>", rep("&nbsp;",12),
                          "conservative = TRUE"),collapse = "")
        }
        if(as.logical(input$detailedoutput)) {
          first = paste(c(first, ",<br>", rep("&nbsp;",12),
                          "detailedoutput = TRUE"),collapse = "")
        }
        first = paste0(c(first,")<br><br>"),collapse="")
      }
      if(input$evaltype == "glm") {
        first = paste0(c(first,
                         "<code style=\"color:#468449\"># Evaluating (Monte Carlo) Design:</code><br>",
                         "eval_design_mc(RunMatrix = design,<br>", rep("&nbsp;",15),
                         "model = ", as.character(as.formula(input$model)), ",<br>", rep("&nbsp;",15),
                         "alpha = ", input$alpha),collapse="")
        if(isblockingtext()) {
          first = paste(c(first, ",<br>", rep("&nbsp;",15),
                          "blocking = TRUE"),collapse = "")
        }
        if(input$nsim != 1000) {
          first = paste(c(first, ",<br>", rep("&nbsp;",15),
                          "nsim = ", input$nsim),collapse = "")
        }
        if(input$glmfamily != "gaussian") {
          first = paste(c(first, ",<br>", rep("&nbsp;",15),
                          "glmfamily = \"",input$glmfamily,"\""),collapse = "")
        }
        if(input$delta != 2) {
          first = paste(c(first, ",<br>", rep("&nbsp;",15),
                          "delta = ",input$delta),collapse = "")
        }
        if(!is.null(input$varianceratios)) {
          first = paste(c(first, ",<br>", rep("&nbsp;",15),
                          "varianceratios = ",input$varianceratio),collapse = "")
        }
        if((input$binomialprobs[1] != 0.4 || input$binomialprobs[2] != 0.6) && input$glmfamily == "binomial") {
          first = paste(c(first, ",<br>", rep("&nbsp;",15),
                          "binomialprobs = c(",input$binomialprobs[1],", ",input$binomialprobs[2]),collapse = "")
        }
        if(as.logical(input$parallel_eval_glm)) {
          first = paste(c(first, ",<br>", rep("&nbsp;",15),
                          "parallel = TRUE"),collapse = "")
        }
        if(as.logical(input$detailedoutput)) {
          first = paste(c(first, ",<br>", rep("&nbsp;",15),
                          "detailedoutput = TRUE"),collapse = "")
        }
        first = paste0(c(first,")<br><br>"),collapse="")
      }
      if(input$evaltype == "surv") {
        first = paste0(c(first,
                         "<code style=\"color:#468449\"># Evaluating (Monte Carlo Survival) Design:</code><br>",
                         "eval_design_survival_mc(RunMatrix = design,<br>", rep("&nbsp;",24),
                         "model = ", as.character(as.formula(input$model)), ",<br>", rep("&nbsp;",24),
                         "alpha = ", input$alpha),collapse="")
        if(input$nsim_surv != 1000) {
          first = paste(c(first, ",<br>", rep("&nbsp;",24),
                          "nsim = ", input$nsim_surv),collapse = "")
        }
        if(input$distribution != "gaussian") {
          first = paste(c(first, ",<br>", rep("&nbsp;",24),
                          "distribution = \"",input$distribution,"\""),collapse = "")
        }
        if(!is.na(input$censorpoint)) {
          first = paste(c(first, ",<br>", rep("&nbsp;",24),
                          "censorpoint = ",input$censorpoint),collapse = "")
        }
        if(input$censortype != "right") {
          first = paste(c(first, ",<br>", rep("&nbsp;",24),
                          "censortype = \"",input$censortype,"\""),collapse = "")
        }
        if(input$delta != 2) {
          first = paste(c(first, ",<br>", rep("&nbsp;",24),
                          "delta = ",input$delta),collapse = "")
        }
        if(as.logical(input$parallel_eval_surv)) {
          first = paste(c(first, ",<br>", rep("&nbsp;",24),
                          "parallel = TRUE"),collapse = "")
        }
        if(as.logical(input$detailedoutput)) {
          first = paste(c(first, ",<br>", rep("&nbsp;",24),
                          "detailedoutput = TRUE"),collapse = "")
        }
        first = paste0(c(first,")<br><br>"),collapse="")
      }
      first = paste0(c(first,"</pre></code>"),collapse="")
      first
    })

    isblocking = reactive({
      input$submitbutton
      any("htc" %in% c(isolate(input$blockdepth1),
                       isolate(input$blockdepth2),
                       isolate(input$blockdepth3),
                       isolate(input$blockdepth4),
                       isolate(input$blockdepth5),
                       isolate(input$blockdepth6))[1:isolate(input$numberfactors)])
    })
    isblockingtext = reactive({
      any("htc" %in% c((input$blockdepth1),
                       (input$blockdepth2),
                       (input$blockdepth3),
                       (input$blockdepth4),
                       (input$blockdepth5),
                       (input$blockdepth6))[1:(input$numberfactors)])
    })

    blockmodel = reactive({
      if(input$model == "~.") {
        as.formula(paste0("~",paste(names(inputlist_htctext()),collapse=" + ")))
      } else {
        names = names(inputlist())
        modelsplit = attr(terms.formula(as.formula(input$model)), "term.labels")
        regularmodel = rep(FALSE, length(modelsplit))
        for(term in names) {
          regex = paste0("(\\b",term,"\\b)|(\\b",term,":)|(:",term,"\\b)|(\\b",term,"\\s\\*)|(\\*\\s",term,"\\b)|(:",term,":)")
          regularmodel = regularmodel | grepl(regex,modelsplit,perl=TRUE)
        }
        paste0("~",paste(modelsplit[!regularmodel],collapse=" + "))
      }
    })

    runmatrix = reactive({
      input$submitbutton
      if(isolate(input$setseed)) {
        set.seed(isolate(input$seed))
      }
      if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
        print("Hard-to-change factors are not currently supported for Alias, T, and G optimal designs.")
      } else {

        if(!isblocking()) {
          gen_design(candidateset = isolate(expand.grid(inputlist())),
                     model = isolate(as.formula(input$model)),
                     trials = isolate(input$trials),
                     optimality = isolate(input$optimality),
                     repeats = isolate(input$repeats),
                     aliaspower = isolate(input$aliaspower),
                     minDopt = isolate(input$mindopt),
                     parallel = isolate(as.logical(input$parallel)))
        } else {
          spd = gen_design(candidateset = isolate(expand.grid(inputlist_htc())),
                           model = isolate(as.formula(blockmodel())),
                           trials = isolate(input$numberblocks),
                           optimality = isolate(input$optimality),
                           repeats = isolate(input$repeats),
                           varianceratio = isolate(input$varianceratio),
                           aliaspower = isolate(input$aliaspower),
                           minDopt = isolate(input$mindopt),
                           parallel = isolate(as.logical(input$parallel)))
          if(isolate(input$trials) %% isolate(input$numberblocks) == 0) {
            sizevector = isolate(input$trials)/isolate(input$numberblocks)
          } else {
            sizevector = c(rep(ceiling(isolate(input$trials)/isolate(input$numberblocks)),isolate(input$numberblocks)))
            unbalancedruns = ceiling(isolate(input$trials)/isolate(input$numberblocks))*isolate(input$numberblocks) - isolate(input$trials)
            sizevector[(length(sizevector)-unbalancedruns+1):length(sizevector)] = sizevector[(length(sizevector)-unbalancedruns+1):length(sizevector)] - 1
          }

          gen_design(candidateset = isolate(expand.grid(inputlist())),
                     model = isolate(as.formula(input$model)),
                     trials = isolate(input$trials),
                     splitplotdesign = spd,
                     splitplotsizes = sizevector,
                     optimality = isolate(input$optimality),
                     repeats = isolate(input$repeats),
                     varianceratio = isolate(input$varianceratio),
                     aliaspower = isolate(input$aliaspower),
                     minDopt = isolate(input$mindopt),
                     parallel = isolate(as.logical(input$parallel)))
        }
      }
    })

    evaluationtype = reactive({
      input$evalbutton
      isolate(input$evaltype)
    })

    powerresults = reactive({
      input$evalbutton
      if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
        print("No design generated")
      } else {
        if(evaluationtype() == "lm") {
          eval_design(RunMatrix = isolate(runmatrix()),
                      model = as.formula(isolate(input$model)),
                      alpha = isolate(input$alpha),
                      blocking = isblocking(),
                      delta = isolate(input$delta),
                      conservative = isolate(input$conservative),
                      detailedoutput = isolate(input$detailedoutput))
        }
      }
    })
    powerresultsglm = reactive({
      input$evalbutton
      if(isolate(input$setseed)) {
        set.seed(isolate(input$seed))
      }
      if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
        print("No design generated")
      } else {
        if(evaluationtype() == "glm") {
          eval_design_mc(RunMatrix = isolate(runmatrix()),
                         model = isolate(as.formula(input$model)),
                         alpha = isolate(input$alpha),
                         blocking = isblocking(),
                         nsim = isolate(input$nsim),
                         varianceratios = isolate(input$varianceratio),
                         glmfamily = isolate(input$glmfamily),
                         delta = isolate(input$delta),
                         binomialprobs = isolate(c(input$binomialprobs[1],input$binomialprobs[2])),
                         detailedoutput = isolate(input$detailedoutput))
        }
      }
    })
    powerresultssurv = reactive({
      input$evalbutton
      if(isolate(input$setseed)) {
        set.seed(isolate(input$seed))
      }
      if(isblocking()) {
        print("Hard-to-change factors are not supported for survival designs. Evaluating design with no blocking.")
      }
      if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
        print("No design generated")
      } else {
        if(evaluationtype() == "surv") {
          eval_design_survival_mc(RunMatrix = isolate(runmatrix()),
                                  model = isolate(as.formula(input$model)),
                                  alpha = isolate(input$alpha),
                                  nsim = isolate(input$nsim),
                                  censorpoint = isolate(input$censorpoint),
                                  censortype = isolate(input$censortype),
                                  distribution = isolate(input$distribution),
                                  delta = isolate(input$delta),
                                  detailedoutput = isolate(input$detailedoutput))
        }
      }
    })


    output$runmatrix = renderTable({
      runmatrix()
    },rownames=TRUE, bordered=TRUE,hover=TRUE,align="c")

    output$powerresults = renderTable({
      powerresults()
    },digits=4,hover=TRUE,align="c")

    output$powerresultsglm = renderTable({
      powerresultsglm()
    },digits=4,hover=TRUE,align="c")

    output$powerresultssurv = renderTable({
      powerresultssurv()
    },digits=4,hover=TRUE,align="c")

    output$aliasplot = renderPlot({
      input$evalbutton
      if(isolate(input$numberfactors) == 1) {
      } else {
        if(isolate(isblocking()) && isolate(input$optimality) %in% c("Alias","T","G")) {
          print("No design generated")
        } else {
          isolate(plot_correlations(runmatrix()))
        }
      }
    })

    output$fdsplot = renderPlot({
      input$evalbutton
      if(isolate(isblocking()) && isolate(input$optimality) %in% c("Alias","T","G")) {
        print("No design generated")
      } else {
        isolate(plot_fds(runmatrix()))
      }
    })

    output$code = renderUI({
      HTML(code())
    })

    output$dopt = renderText({
      input$submitbutton
      isolate(substr(attr(runmatrix(),"D"),5,nchar(attr(runmatrix(),"D"))))
    })
    output$aopt = renderText({
      input$submitbutton
      isolate(substr(attr(runmatrix(),"A"),5,nchar(attr(runmatrix(),"A"))))
    })
    output$iopt = renderText({
      input$submitbutton
      isolate(attr(runmatrix(),"I"))
    })
    output$eopt = renderText({
      input$submitbutton
      isolate(attr(runmatrix(),"E"))
    })
    output$gopt = renderText({
      input$submitbutton
      isolate(attr(runmatrix(),"G"))
    })
    output$topt = renderText({
      input$submitbutton
      isolate(attr(runmatrix(),"T"))
    })
    output$optimalsearch = renderPlot({
      input$submitbutton
      isolate(plot(attr(runmatrix(),"optimalsearchvalues"),xlab="Search Iteration",ylab="Criteria Value",type = 'p', col = 'red', pch=16))
      isolate(points(x=attr(runmatrix(),"best"),y=attr(runmatrix(),"optimalsearchvalues")[attr(runmatrix(),"best")],type = 'p', col = 'green', pch=16,cex =2))
    })
    output$simulatedpvalues = renderPlot({
      input$evalbutton
      pvalrows = isolate(floor(ncol(attr(powerresultsglm(),"pvals"))/3)+1)
      if(!is.null(attr(powerresultsglm(),"pvals"))) {
        par(mfrow=c(pvalrows,3))
        for(col in 1:isolate(ncol(attr(powerresultsglm(),"pvals")))) {
          isolate(hist(attr(powerresultsglm(),"pvals")[,col],breaks=seq(0,1,0.05),main = colnames(attr(powerresultsglm(),"pvals"))[col],xlim=c(0,1),xlab="p values",ylab="Count", col = 'red', pch=16))
        }
      }
    })
    output$parameterestimates = renderPlot({
      input$evalbutton
      if(!is.null(attr(powerresultsglm(),"estimates"))) {
        ests = apply(attr(powerresultsglm(),"estimates"),2,quantile,c(0.05,0.5,0.95))

        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        plot(x=1:length(colnames(ests)),y=ests[2,],ylim=c(min(as.vector(ests)),max(as.vector(ests))),
             xaxt = "n",
             xlab = "Parameters",
             ylab = "Parameter Estimates",
             xlim=c(0.5,length(colnames(ests))+0.5),type="p",pch=16,col="red",cex=1)
        axis(1,at=1:length(colnames(ests)),labels=colnames(ests), las = 2)
        legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
        grid(nx=NA,ny=NULL)
        arrows(x0=1:length(colnames(ests)),y0=ests[1,],x1=1:length(colnames(ests)),y1=ests[3,],length=0.05,angle=90,code=3)
        points(x=1:length(colnames(ests)),y=attr(powerresultsglm(),"anticoef"),pch=16,col="blue",cex=1)
        title("Simulated Parameter Estimates (5%-95% Confidence Intervals)")
      }
    })
    output$responsehistogram = renderPlot({
      input$evalbutton
      if(!is.null(attr(powerresultsglm(),"estimates"))) {
        responses = as.vector(attr(powerresultsglm(),"estimates") %*% t(attr(powerresultsglm(),"modelmatrix")))
        if(isolate(input$glmfamily) == "binomial") {
          responses = exp(responses)/(1+exp(responses))
          hist(responses,breaks=isolate(input$nsim)*isolate(input$trials)/10,xlim=c(0,1),xlab="Response (Probability)")
          grid(nx=NA,ny=NULL)
          hist(responses,breaks=isolate(input$nsim)*isolate(input$trials)/10,add=TRUE,main="Distribution of Simulated Responses",xlab="Response (Probability)",xlim=c(0,1),ylab="Count",col = "red",border="red")
        } else {
          hist(responses,breaks=isolate(input$nsim)*isolate(input$trials)/10,xlab="Response")
          grid(nx=NA,ny=NULL)
          hist(responses,breaks=isolate(input$nsim)*isolate(input$trials)/10,add=TRUE,main="Distribution of Simulated Responses",xlab="Response",ylab="Count",col = "red",border="red")
        }
      }
    })
    output$responsehistogramsurv = renderPlot({
      input$evalbutton
      if(!is.null(attr(powerresultssurv(),"estimates"))) {
        responses = as.vector(attr(powerresultssurv(),"estimates") %*% t(attr(powerresultssurv(),"modelmatrix")))
        hist(responses,breaks=isolate(input$nsim)*isolate(input$trials)/10,xlab="Response")
        grid(nx=NA,ny=NULL)
        hist(responses,breaks=isolate(input$nsim)*isolate(input$trials)/10,add=TRUE,main="Distribution of Simulated Responses (from survival analysis)",xlab="Response",ylab="Count",col = "red",border="red")
      }
    })
    output$separationwarning = renderText({
      input$evalbutton
      likelyseparation = FALSE
      if(isolate(input$evaltype) == "glm" && isolate(input$glmfamily) == "binomial") {
        if(!is.null(attr((powerresultsglm()),"pvals"))) {
          pvalmat = attr((powerresultsglm()),"pvals")
          for(i in 2:ncol(pvalmat)) {
            pvalcount = hist(pvalmat[,i],breaks=seq(0,1,0.05),plot=FALSE)
            likelyseparation = likelyseparation || (all(pvalcount$count[20] > pvalcount$count[17:19]) && pvalcount$count[20] > isolate(input$nsim)/10)
          }
        }
      }
      if(likelyseparation) {
        "<p style=\"color: #F00;\">Partial or complete separation likely detected in your binomial Monte Carlo simulation. Increase the number of runs in your design or decrease the number of model parameters to improve power.</p>"
      } else {
        ""
      }
    })
    outputOptions(output,"separationwarning", suspendWhenHidden=FALSE)
  }

  runGadget(shinyApp(ui, server),viewer = dialogViewer(dialogName = "skprGUI", width = 1200,height=1000))
}
