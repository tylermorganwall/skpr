#' Generate Factor Input Panel
#'
#' @param factor_n Factor number
#'
#' @return Shiny UI
#' @keywords internal
generate_factor_input_panel = function(factor_n = 1, factor_input_cache = NULL) {
  panelstyle = "background-color: rgba(86, 96, 133, 0.3);
  border-radius: 15px;
  -webkit-box-shadow: inset 0px 0px 10px 4px rgba(41, 49, 83, 0.42);
  box-shadow: inset 0px 0px 10px 2px rgba(41, 49, 83, 0.42);
  padding-top: 15px;
  padding-bottom: 10px;
  color: rgb(255, 255, 255);
  border: 0px;"

  factorname_n = sprintf("factorname%i",factor_n)
  factortype_n = sprintf("factortype%i",factor_n)
  numericlow_n = sprintf("numericlow%i",factor_n)
  numerichigh_n = sprintf("numerichigh%i",factor_n)
  numericlength_n = sprintf("numericlength%i",factor_n)
  disclevels_n = sprintf("disclevels%i",factor_n)
  levels_n = sprintf("levels%i",factor_n)
  blockdepth_n = sprintf("blockdepth%i",factor_n)
  default_val = function(input, val) ifelse(length(input) == 0, val, input)
  if(factor_n > 1) {
    single_panel = shiny::wellPanel(style = panelstyle,
      shiny::h3(sprintf("Factor %i", factor_n)),
      shiny::fluidRow(
        shiny::column(width = 5,
                      shiny::selectInput(inputId = sprintf("blockdepth%i",factor_n),
                                         choices = list("Easy" = "etc", "Hard" = "htc"),
                                         selected = default_val(factor_input_cache[[blockdepth_n]], "Easy") ,
                                         label = "Changes")
        ),
        shiny::column(width = 7,
                      shiny::selectInput(inputId = sprintf("factortype%i",factor_n),
                                         choices = list("Continuous" = "numeric", "Categorical" = "cat", "Discrete Numeric" = "discnum"),
                                         selected = default_val(factor_input_cache[[factortype_n]], "Continuous"),
                                         label = "Type")
        )
      ),
      shiny::fluidRow(
        shiny::column(width = 12,
                      shiny::textInput(inputId = sprintf("factorname%i",factor_n),
                                       value = default_val(factor_input_cache[[factorname_n]], sprintf("X%i", factor_n)),
                                       label = "Name")
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input.factortype%i == \'numeric\'",factor_n),
        shiny::fluidRow(
          shiny::column(width = 4,
                        shiny::numericInput(inputId = sprintf("numericlow%i",factor_n),
                                            value =  default_val(factor_input_cache[[numericlow_n]], -1),
                                            label = "Low")
          ),
          shiny::column(width = 4,
                        shiny::numericInput(inputId = sprintf("numerichigh%i",factor_n),
                                            value = default_val(factor_input_cache[[numerichigh_n]], 1),
                                            label = "High")
          ),
          shiny::column(width = 4,
                        shiny::numericInput(inputId = sprintf("numericlength%i",factor_n),
                                            value = default_val(factor_input_cache[[numericlength_n]], 3),
                                            min = 2,
                                            step = 1,
                                            label = "Breaks")
          )
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input.factortype%i == \'discnum\'",factor_n),
        shiny::fluidRow(
          shiny::column(width = 12,
                        shiny::textInput(inputId = sprintf("disclevels%i",factor_n),
                                         value = default_val(factor_input_cache[[disclevels_n]], ""),
                                         label = "Levels (separate with commas)")
          )
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input.factortype%i == \'cat\'",factor_n),
        shiny::fluidRow(
          shiny::column(width = 12,
                        shiny::textInput(inputId = sprintf("levels%i",factor_n),
                                         value = default_val(factor_input_cache[[levels_n]], "a, b, c"),
                                         label = "Levels (separate with commas)")
          )
        )
      )
    )
  } else {
    single_panel = shiny::wellPanel(style = panelstyle,
      shiny::h3(sprintf("Factor %i", factor_n)),
      shiny::fluidRow(
        shiny::column(width = 5,
                      shiny::selectInput(inputId = sprintf("blockdepth%i",factor_n),
                                         choices = list("Easy" = "etc", "Hard" = "htc"),
                                         label = "Changes")
        ),
        shiny::column(width = 7,
                      shiny::selectInput(inputId = sprintf("factortype%i",factor_n),
                                         choices = list("Continuous" = "numeric", "Categorical" = "cat", "Discrete Numeric" = "discnum"),
                                         label = "Type")
        )
      ),
      shiny::fluidRow(
        shiny::column(width = 12,
                      shiny::textInput(inputId = sprintf("factorname%i",factor_n),
                                       value = "X1",
                                       label = "Name")
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input.factortype%i == \'numeric\'",factor_n),
        shiny::fluidRow(
          shiny::column(width = 4,
                        shiny::numericInput(inputId = sprintf("numericlow%i",factor_n),
                                            value =  -1,
                                            label = "Low")
          ),
          shiny::column(width = 4,
                        shiny::numericInput(inputId = sprintf("numerichigh%i",factor_n),
                                            value = 1,
                                            label = "High")
          ),
          shiny::column(width = 4,
                        shiny::numericInput(inputId = sprintf("numericlength%i",factor_n),
                                            value = 3,
                                            min = 2,
                                            step = 1,
                                            label = "Breaks")
          )
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input.factortype%i == \'discnum\'",factor_n),
        shiny::fluidRow(
          shiny::column(width = 12,
                        shiny::textInput(inputId = sprintf("disclevels%i",factor_n),
                                         value = "",
                                         label = "Levels (separate with commas)")
          )
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input.factortype%i == \'cat\'",factor_n),
        shiny::fluidRow(
          shiny::column(width = 12,
                        shiny::textInput(inputId = sprintf("levels%i",factor_n),
                                         value = "a, b, c",
                                         label = "Levels (separate with commas)")
          )
        )
      )
    )
  }
  return(single_panel)
}

#' Generate Block Panel
#'
#' @param any_htc Factor number
#'
#' @return Shiny UI
#' @keywords internal
generate_block_panel = function(any_htc) {
  if(any_htc) {
    block_panel = shiny::fluidRow(
      shiny::column(width = 12, shiny::numericInput(inputId = "numberblocks",
                                                    4, label = "Number of blocks"))
    )
    return(block_panel)
  }
}

#' Generate Optimality Results
#'
#' @param any_htc Factor number
#'
#' @return Shiny UI
#' @keywords internal
generate_optimality_results = function(any_htc) {
  textOutput = shiny::textOutput
  if(!any_htc) {
    opt_display = shiny::column(width = 6,
                                shiny::h3("Criteria"),
                                shiny::h4("D"),
                                textOutput(outputId = "dopt"),
                                shiny::h4("A"),
                                textOutput(outputId = "aopt"),
                                shiny::h4("I (Average prediction variance)"),
                                textOutput(outputId = "iopt"),
                                shiny::h4("E"),
                                textOutput(outputId = "eopt"),
                                shiny::h4("G"),
                                textOutput(outputId = "gopt"),
                                shiny::h4("T"),
                                textOutput(outputId = "topt")
    )
  } else {
    opt_display = shiny::column(width = 6,
                                shiny::h4("I (Average prediction variance)"),
                                textOutput(outputId = "iopt")
    )
  }
  return(opt_display)
}
