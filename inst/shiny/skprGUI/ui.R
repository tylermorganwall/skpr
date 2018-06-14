library(shinythemes)
library(rintrojs)

#ui.R for skprGUI

panelstyle = "background-color: rgba(86, 96, 133, 0.3);
                border-radius: 15px;
                -webkit-box-shadow: inset 0px 0px 10px 4px rgba(41, 49, 83, 0.42);
                box-shadow: inset 0px 0px 10px 2px rgba(41, 49, 83, 0.42);
                padding-top: 15px;
                padding-bottom: 10px;
                color: rgb(255, 255, 255);
                border: 0px;"

function(request) {
  fluidPage(theme = shinytheme("yeti"),
            shinyjs::useShinyjs(),
            introjsUI(),
            HTML("<style> table {font-size: 14px;}
                 .btn2 {
                 color: #fff;
                 border-color: rgba(46, 109, 164, 0);
                 background: linear-gradient(to bottom, rgb(64, 108, 221) 0%, rgb(107, 167, 223) 100%);
                 border-radius: 11px;
                 -webkit-box-shadow: 0px 2px 0px 0px rgb(59, 76, 145);
                 box-shadow: 0px 2px 0px 0px rgb(59, 76, 145);
                 }
                 .btn2.disabled, .btn2.disabled:hover,.btn2.disabled:active,.btn2.disabled:focus {
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
                 transition: background-color 0.2s ease-in-out,color 0.2s ease-in-out,border 0.2s ease-in-out;
                 }
                 .nav-tabs>li>a:active {
                 background-color: rgb(184, 255, 181);
                 border: 1px solid rgb(184, 255, 181);
                 color: #000000;
                 transition: background-color 0.2s ease-in-out,color 0.2s ease-in-out,border 0.2s ease-in-out;
                 }
                 .nav-tabs>li>a:focus {
                 background-color: rgb(184, 255, 181);
                 border: 1px solid rgb(184, 255, 181);
                 color: #000000;
                 transition: background-color 0.2s ease-in-out,color 0.2s ease-in-out,border 0.2s ease-in-out;
                 }

                 .nav-tabs>li>a:hover {
                 background-color: rgb(184, 255, 181);
                 border: 1px solid rgb(184, 255, 181);
                 color: #000000;
                 transition: background-color 0.2s ease-in-out,color 0.2s ease-in-out,border 0.2s ease-in-out;
                 }
                 .nav-tabs>li.active>a {
                 background-color: rgb(184, 255, 181);
                 border: 1px solid rgb(184, 255, 181);
                 color: #000000;
                 transition: background-color 0.2s ease-in-out,color 0.2s ease-in-out,border 0.2s ease-in-out;
                 }
                 .nav-tabs>li.active>a:focus {
                 background-color: rgb(184, 255, 181);
                 border: 1px solid rgb(184, 255, 181);
                 color: #000000;
                 transition: background-color 0.2s ease-in-out,color 0.2s ease-in-out,border 0.2s ease-in-out;
                 }
                 .nav-tabs>li.active>a:hover {
                 background-color: rgb(184, 255, 181);
                 border: 1px solid rgb(184, 255, 181);
                 color: #000000;
                 transition: background-color 0.2s ease-in-out,color 0.2s ease-in-out,border 0.2s ease-in-out;
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
            sidebarLayout(
              sidebarPanel(tags$style(".well {background-color:#a1b0da;
                                      border: 1px solid #a1b0da;
                                      border-radius: 13px;
                                      -webkit-box-shadow: 0px 0px 10px 5px rgba(0, 0, 0, 0.15);
                                      box-shadow: 0px 0px 10px 5px rgba(0, 0, 0, 0.15);}"),
                           HTML("<h1 style='margin-top: 0px;'>skpr<strong style='color: black;'>GUI</strong></h1>"),
                           hr(),
                           introBox(fluidRow(
                             column(width=6,
                                    actionButton("submitbutton", HTML("<strong>Generate <br>Design</strong>"),
                                                 class="btn2")
                             ),
                             column(width=6,
                                    actionButton("evalbutton", HTML("<strong>Evaluate <br>Design</strong>"),
                                                 class="btn2")
                             )
                           ), data.step = 1, data.intro="<h3><center>Welcome to skpr!</h3></center> This tutorial will walk you through all of the features of the GUI and teach you how to create and analyze an experimental design. All features seen in the GUI can be easily recreated in the console, and skpr provides the full script used to do that, based on your inputs. Additional advanced capabilities not available in the GUI can be accessed via the code. <b>Let's get started!</b> <br><br>Click these buttons to generate a new design, or re-run a new design evaluation with updated parameters."),
                           tabsetPanel(
                             tabPanel(
                               "Basic",
                               introBox(numericInput(inputId = "trials",
                                                     12, label = "Trials"), data.step = 2, data.intro = "This is the number of runs in the experiment."),
                               introBox(textInput(inputId = "model",
                                                  "~.", label = "Model"), data.step = 3, data.intro = "This is the model. <br><br> <b>~.</b> produces a linear model for all terms with no interactions. <br><br> Interactions can be added with the colon operator: <br><br> <b>~X1 + X2 + X1:X2</b> <br><br> and quadratic effects with an I() (as in India): <br><br><b>~X1 + X2 + I(X1^2)</b>."),
                               conditionalPanel(condition = "(input.blockdepth1 == 'htc') ||
                                                (input.blockdepth2 == 'htc' && input.numberfactors > 1) ||
                                                (input.blockdepth3 == 'htc' && input.numberfactors > 2) ||
                                                (input.blockdepth4 == 'htc' && input.numberfactors > 3) ||
                                                (input.blockdepth5 == 'htc' && input.numberfactors > 4) ||
                                                (input.blockdepth6 == 'htc' && input.numberfactors > 5)",
                                                fluidRow(
                                                  column(width=12,numericInput(inputId = "numberblocks",
                                                                               4, label = "Number of blocks"))
                                                )
                               ),
                               conditionalPanel(condition = "input.numberfactors == 6",
                                                fluidRow(
                                                  column(width=12,
                                                         HTML("<p style=\"color: #000;\">skprGUI only supports up to 6 factors. Alter the generated code to add more.</p>")
                                                  )
                                                )
                               ),
                               introBox(numericInput(inputId = "numberfactors",
                                                     min=1,max=6, 1, label = "Number of Factors"), data.step = 4, data.intro = "This is the number of factors in the experiment. skprGUI supports up to 6 factors, but the underlying code supports any number of factors by calling the code directly. If you require more factors, use the generating code as a template and add more terms to the candidate set."),
                               br(),
                               introBox(wellPanel(style = panelstyle,
                                                  h3("Factor 1"),
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
                                                                          min = 2,
                                                                          step = 1,
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
                               ), data.step = 5, data.intro = "This pane allows you to change the factor type, specify categorical and discrete numeric levels, and make factors hard-to-change. If numeric, specify the highest and lowest values and the number of breaks between. If categorical or discrete numeric, specify levels separated by commas."),
                               conditionalPanel(
                                 condition = "input.numberfactors > 1",
                                 wellPanel(style = panelstyle,
                                           h3("Factor 2"),
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
                                                                   min = 2,
                                                                   step = 1,
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
                                 wellPanel(style = panelstyle,
                                           h3("Factor 3"),
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
                                                                   min = 2,
                                                                   step = 1,
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
                                 wellPanel(style = panelstyle,
                                           h3("Factor 4"),
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
                                                                   min = 2,
                                                                   step = 1,
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
                                 wellPanel(style = panelstyle,
                                           h3("Factor 5"),
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
                                                                   min = 2,
                                                                   step = 1,
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
                                 wellPanel(style = panelstyle,
                                           h3("Factor 6"),
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
                                                                   min = 2,
                                                                   step = 1,
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
                                    introBox(selectInput(inputId = "optimality",
                                                         choices = c("D","I","A","Alias","G","E","T"),
                                                         label = "Optimality"),data.step = 6, data.intro = "Change the optimality criterion. If Alias-optimal selected, additional Alias-optimal specific options (minimum D-optimality and Alias-interaction level) will become available to change."),
                                    introBox(numericInput(inputId = "repeats",
                                                          20, label = "Repeats"),data.step = 7, data.intro = "Changes the depth of the optimal design search. Increasing this will increase the probability that an optimal design is found."),
                                    introBox(numericInput(inputId = "varianceratio",
                                                          1, label = "Variance Ratio"), data.step = 8, data.intro = "The ratio of the variance between whole plots and subplots for split-plot designs."),
                                    conditionalPanel(
                                      condition = "input.optimality == \'Alias\'",
                                      numericInput(inputId = "aliaspower",
                                                   min=2,value=2, label = "Alias Optimal Interaction Level"),
                                      sliderInput(inputId = "mindopt",
                                                  min=0,max=1,value=0.8, label = "Minimum D Optimality")
                                    ),
                                    introBox(checkboxInput(inputId = "setseed",
                                                           label = "Set Random Number Generator Seed",
                                                           value=FALSE),data.step = 9, data.intro = "Set the random seed for both design generation and evaluation. This allows for completely reproducible designs and Monte Carlo simulations."),
                                    conditionalPanel(
                                      condition = "input.setseed",
                                      numericInput(inputId = "seed",
                                                   1, label = "Random Seed")
                                    ),
                                    introBox(checkboxInput(inputId = "parallel",
                                                           label = "Parallel Search",
                                                           value = FALSE), data.step = 10, data.intro = "Use all available cores to compute design. Only set to true if the design search is taking >10 seconds to finish. Otherwise, the overhead in setting up the parallel computation outweighs the speed gains."),
                                    introBox(checkboxInput(inputId = "splitanalyzable",
                                                           label = "Include Blocking Columns in Run Matrix",
                                                           value=TRUE), data.step=11, data.intro = "Convert row structure to blocking columns. This is required for analyzing the split-plot structure using REML."),
                                    introBox(checkboxInput(inputId = "detailedoutput",
                                                           label = "Detailed Output",
                                                           value=FALSE), data.step=12, data.intro = "Outputs a tidy data frame of additional design information, including anticipated coefficients and design size."),
                                    introBox(checkboxInput(inputId = "advanceddiagnostics",
                                                           label = "Advanced Design Diagnostics",
                                                           value=TRUE), data.step=13, data.intro = "Outputs additional information about the optimal search and advanced Monte Carlo information. This includes a list of all available optimal criteria, a plot of the computed optimal values during the search (useful for determining if the repeats argument should be increased), and a histogram of p-values for each parameter in Monte Carlo simulations."),
                                    selectInput(inputId = "colorchoice",choices = c("Default"="D","Magma"="A","Inferno"="B","Plasma"="C","None"="none"), label = "Color")
                           ),
                           tabPanel("Power",
                                    introBox(introBox(introBox(radioButtons(inputId = "evaltype",
                                                                            label="Model Type",
                                                                            choiceNames = c("Linear Model","Generalized Linear Model","Survival Model"),
                                                                            choiceValues = c("lm","glm","surv")), data.step=14, data.intro = "Change the type of analysis. Linear model calculates power with parametric assumptions, while the Generalized Linear Model and Survival Model both calculate power using a Monte Carlo approach."),
                                                      data.step=18,data.intro="Changing the evaluation type to a GLM Monte Carlo reveals several additional controls."),
                                             data.step=22,data.intro="Survival analysis Monte Carlo power generation. This simulates data according to the design, and then censors the data if it is above or below a user defined threshold. This simulation is performed with the survreg package."),
                                    introBox(sliderInput(inputId = "alpha",
                                                         min=0,max=1,value=0.05, label = "Alpha"), data.step=15, data.intro = "Specify the acceptable Type-I error (false positive rate)"),
                                    conditionalPanel(
                                      condition = "input.evaltype == \'lm\' || (input.evaltype == \'glm\' && input.glmfamily == \'gaussian\') || (input.evaltype == \'surv\' && (input.distribution == \'gaussian\' || input.distribution == \'lognormal\'))",
                                      introBox(numericInput(inputId = "snr",
                                                            value=2, step=0.1, label = "SNR"), data.step=16, data.intro = "Signal-to-noise ratio for linear models.")
                                    ),
                                    conditionalPanel(
                                      condition = "input.evaltype == \'glm\' && input.glmfamily == \'poisson\'",
                                      fluidRow(
                                        column(width=6,
                                               numericInput(inputId = "poislow", "Low # of Events:",
                                                            min = 0, value=1)
                                        ),
                                        column(width=6,
                                               numericInput(inputId = "poishigh", "High # of Events:",
                                                            min = 0, value=2)
                                        )
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "(input.evaltype == \'glm\' && input.glmfamily == \'exponential\') || (input.evaltype == \'surv\' && input.distribution == \'exponential\')",
                                      fluidRow(
                                        column(width=6,
                                               numericInput(inputId = "explow", "Low Mean:",
                                                            min = 0, value=1)
                                        ),
                                        column(width=6,
                                               numericInput(inputId = "exphigh", "High Mean:",
                                                            min = 0, value=2)
                                        )
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.evaltype == \'glm\' && input.glmfamily == \'binomial\'",
                                      sliderInput(inputId = "binomialprobs", "Binomial Probabilities:",
                                                  min = 0, max = 1, value = c(0.4,0.6))
                                    ),
                                    introBox(conditionalPanel(
                                      condition = "input.evaltype == \'lm\'",
                                      checkboxInput(inputId = "conservative",
                                                    label = "Conservative Power",
                                                    value=FALSE)
                                    ), data.step=17, data.intro = "Calculates conservative effect power for 3+ level categorical factors. Calculates power once, and then sets the anticipated coefficient corresponding to the highest power level in each factor to zero. The effect power for those factors then show the most conservative power estimate."),
                                    conditionalPanel(
                                      condition = "input.evaltype == \'glm\'",
                                      introBox(numericInput(inputId = "nsim",
                                                            value=1000,
                                                            label = "Number of Simulations"), data.step=19, data.intro = "The number of Monte Carlo simulations to run. More simulations will result in a more precise power estimation."),
                                      introBox(selectInput(inputId = "glmfamily",
                                                           choices = c("gaussian","binomial","poisson","exponential"),
                                                           label = "GLM Family"), data.step=20, data.intro = "The distributional family used in the generalized linear model. If binomial, an additional slider will appear allowing you to change the desired upper and lower probability bounds. This automatically calculates the anticipated coefficients that correspond to that probability range."),
                                      introBox(checkboxInput(inputId = "parallel_eval_glm",
                                                             label = "Parallel Evaluation",
                                                             value=FALSE), data.step=21, data.intro = "Turn on multicore support for evaluation. Should only be used if the calculation is taking >10s to complete. Otherwise, the overhead in setting up the parallel computation outweighs the speed gains.")
                                    ),
                                    conditionalPanel(
                                      condition = "input.evaltype == \'surv\'",
                                      numericInput(inputId = "nsim_surv",
                                                   value=1000,
                                                   label = "Number of Simulations"),
                                      selectInput(inputId = "distribution",
                                                  choices = c("gaussian","lognormal","exponential"),
                                                  label = "Distribution"),
                                      introBox(numericInput(inputId = "censorpoint",
                                                            value=NA,
                                                            label = "Censor Point"),data.step=23, data.intro = "The value after (if right censored) or before (if left censored) data will be censored. The default is no censoring."),
                                      introBox(selectInput(inputId = "censortype",
                                                           choices = c("right","left"),
                                                           label = "Censoring Type"), data.step=24, data.intro = "The type of censoring."),
                                      checkboxInput(inputId = "parallel_eval_surv",
                                                    label = "Parallel",
                                                    value=FALSE)
                                    )
                           )
            )
              ),
            mainPanel(fluidRow(
              column(width=6,h1("Results")),
              column(width=2),
              column(width=2,introBox(bookmarkButton(label="Save State",title="Generates a URL that encodes the current state of the application for easy sharing and saving of analyses. Paste this URL into a browser (possible changing the port and address if locally different) to restore the state of the application. Be sure to set a random seed before bookmarking to recover the same results."), class="bookmark",data.step=33, data.intro = "Generates a URL that encodes the current state of the application for easy sharing and saving of analyses. Paste this URL into a browser (possible changing the port and address if locally different) to restore the state of the application. Be sure to set a random seed before bookmarking to recover the same results.")),
              column(width=2,actionButton(inputId = "tutorial","Tutorial")),
              tags$style(type='text/css', "#tutorial {margin-top: 25px;} .bookmark {margin-top: 25px;}")
            ),
            tabsetPanel(
              tabPanel("Design",
                       h2("Design"),
                       checkboxInput(inputId = "orderdesign",label = "Order Design",value=FALSE),
                       introBox(tableOutput(outputId = "runmatrix"),data.step = 25, data.intro = "The generated optimal design. If hard-to-change factors are present, there will be an additional blocking column specifying the block number. Here, we have generated a design with three factors and 12 runs."),
                       hr()
              ),
              tabPanel("Design Evaluation",
                       introBox(fluidRow(
                         column(width=6,
                                h2("Power Results"),
                                conditionalPanel(
                                  condition = "input.evaltype == \'lm\'",
                                  tableOutput(outputId = "powerresults")
                                ),
                                introBox(conditionalPanel(
                                  condition = "input.evaltype == \'glm\'",
                                  tableOutput(outputId = "powerresultsglm")
                                ),data.step=27,data.intro = "The power of the design. Output is a tidy data frame of the power and the type of evaluation for each parameter. If the evaluation type is parametric and there are 3+ level categorical factors, effect power will also be shown. Here, we have our GLM simulated power estimation."),
                                conditionalPanel(
                                  condition = "input.evaltype == \'surv\'",
                                  tableOutput(outputId = "powerresultssurv")
                                )
                         )
                       ),data.step = 26,data.intro = "This page shows the calculated/simulated power, as well as other design diagnostics. (results may take a second to appear)"),
                       hr(),
                       fluidRow(align="center",
                                column(width=6,
                                       h3("Correlation Map"),
                                       introBox(conditionalPanel("input.numberfactors > 1",
                                                                 plotOutput(outputId = "aliasplot")),data.step=28,data.intro = "Correlation map of the design. This shows the correlation structure between main effects and their interactions. Ideal correlation structures will be diagonal (top left to bottom right). Alias-optimal designs minimize the elements of this matrix that correspond to a main effects term interacting with an interaction term."),
                                       conditionalPanel("input.numberfactors == 1",
                                                        br(),
                                                        br(),
                                                        br(),
                                                        br(),
                                                        br(),
                                                        br(),
                                                        br(),
                                                        br(),
                                                        HTML("<font color=#898989> One Parameter: <br>No Correlation Map</font>"))
                                ),
                                column(width=6,
                                       h3("Fraction of Design Space"),
                                       introBox(plotOutput(outputId = "fdsplot"), data.step=29,data.intro = "Fraction of design space plot. The horizontal line corresponds to the average prediction variance for the design.")
                                )
                       ),
                       conditionalPanel(
                         condition = "input.evaltype == \'glm\'",
                         fluidRow(
                           hr(),
                           column(width=12,
                                  h3("Simulated Response Estimates"),
                                  introBox(plotOutput(outputId = "responsehistogram"),data.step=30, data.intro = "Distribution of response estimates for Monte Carlo simulations. For a given design and distributional family, this plot shows the model's estimates of the overall response of the experiment (red) with the actual values on top (blue). ")
                           ),
                           conditionalPanel(
                             condition = "input.glmfamily != \'binomial\'",
                             column(width=6,
                                    numericInput(inputId = "estimatesxminglm",
                                                 value=NA,
                                                 label = "x-min")
                             ),
                             column(width=6,
                                    numericInput(inputId = "estimatesxmaxglm",
                                                 value=NA,
                                                 label = "x-max")
                             )
                           )
                         )
                       ),
                       conditionalPanel(
                         condition = "input.evaltype == \'surv\'",
                         fluidRow(
                           hr(),
                           column(width=12,
                                  h3("Simulated Response Estimates"),
                                  plotOutput(outputId = "responsehistogramsurv")
                           ),
                           column(width=6,
                                  numericInput(inputId = "estimatesxminsurv",
                                               value=NA,
                                               label = "x-min")
                           ),
                           column(width=6,
                                  numericInput(inputId = "estimatesxmaxsurv",
                                               value=NA,
                                               label = "x-max")
                           )
                         )
                       ),
                       conditionalPanel(
                         condition = "input.evaltype == \'glm\'",
                         fluidRow(
                           hr(),
                           column(width=12,
                                  h3("Simulated Estimates"),
                                  introBox(plotOutput(outputId = "parameterestimates"),data.step=31, data.intro = "Individual parameter estimates for each of the design factors. The 95% confidence intervals are extracted from the actual simulated values.")
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
                       introBox(htmlOutput(outputId = "code"),data.step=32,data.intro="The R code used to generate the design and evaluate power. This section is updated in real time as the user changes the inputs. Copy and paste this code at the end to easily save, distribute, and reproduce your results. This also provides an easy code template to automate more complex design searches not built in to the GUI. Also included is the code showing how to analyze the experiment once the data has been collected, for all supported types of analyses. ")
              )
            )
            )
              )
            )
}
