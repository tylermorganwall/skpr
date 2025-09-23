#' Build Code Pane HTML
#'
#' @param state Named list containing `input` and reactive helpers used to render the code pane.
#' @return HTML string for the code pane.
#' @keywords internal
build_code_pane_html <- function(state) {
  input <- state$input
  inputstring <- state$inputstring
  any_htc <- state$any_htc
  blockmodel <- state$blockmodel
  regularmodelstring <- state$regularmodelstring
  modelwithblocks <- state$modelwithblocks
  isblockingtext <- state$isblockingtext
  anyfactors <- state$anyfactors
  contraststring <- state$contraststring
  effectsize <- state$effectsize

  shiny::req(inputstring(), cancelOutput = TRUE)
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
}
#' Create Code Pane Reactives
#'
#' @param input Shiny input object.
#' @param update Reactive dependency used to trigger recomputation.
#' @param candidatesetall Reactive that returns the full candidate set list.
#' @param any_htc_fn Function returning whether any factors are hard-to-change.
#'
#' @return List of reactives required exclusively by the code pane.
#' @keywords internal
create_code_pane_reactives <- function(
  input,
  update,
  candidatesetall,
  any_htc_fn
) {
  regularmodelstring <- shiny::reactive({
    tryCatch(
      {
        if (
          any(
            unlist(strsplit(
              as.character(stats::as.formula(input$model)[2]),
              "\\s\\+\\s|\\s\\*\\s|\\:"
            )) == "."
          )
        ) {
          dotreplace = paste0(
            "(",
            paste0(names(candidatesetall()), collapse = " + "),
            ")"
          )
          additionterms = unlist(strsplit(
            as.character(stats::as.formula(input$model)[2]),
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
        paste0(as.character(stats::as.formula(stringmodel)), collapse = "")
      },
      error = function(e) {
        paste0(input$model, collapse = "")
      }
    )
  })

  inputstring <- shiny::reactive({
    shiny::req(update, cancelOutput = TRUE)
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
      shiny::req(input[[factortype_n]], cancelOutput = TRUE)
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

  isblockingtext <- shiny::reactive({
    any_htc_fn()
  })

  modelwithblocks <- shiny::reactive({
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

  contraststring <- shiny::reactive({
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

  anyfactors <- shiny::reactive({
    fac = FALSE
    for (i in seq_len(input$numberfactors)) {
      factortype_n = sprintf("factortype%i", i)
      if (input[[factortype_n]] == "cat") {
        fac = TRUE
      }
    }
    fac
  })

  list(
    regularmodelstring = regularmodelstring,
    inputstring = inputstring,
    isblockingtext = isblockingtext,
    modelwithblocks = modelwithblocks,
    contraststring = contraststring,
    anyfactors = anyfactors
  )
}
