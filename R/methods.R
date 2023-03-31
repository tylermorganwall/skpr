#'@title Print evaluation information
#'
#'@description Prints design evaluation information below the data.frame of power values
#'
#'Note: If options("skpr.ANSI") is `NULL` or `TRUE`, ANSI codes will be used during printing
#'to prettify the output. If this is `FALSE`, only ASCII will be used.
#'
#'@param x The x of the evaluation functions in skpr
#'@param ... Additional arguments.
#'@import graphics grDevices
#'@export
#'@examples
#'#Generate/evaluate a design and print its information
#'factorialcoffee = expand.grid(cost = c(1, 2),
#'                               type = as.factor(c("Kona", "Colombian", "Ethiopian", "Sumatra")),
#'                               size = as.factor(c("Short", "Grande", "Venti")))
#'
#'designcoffee = gen_design(factorialcoffee,
#'                          ~cost + size + type, trials = 29, optimality = "D", repeats = 100)
#'
#'eval_design(designcoffee)
print.skpr_eval_output = function(x, ...) {
  class(x) = "data.frame"
  print(x)
  if((is.null(getOption("skpr.ANSI")) || getOption("skpr.ANSI")) && !is_rendering_in_knitr()) {
    boldstart = "\u001b[1m"
    formatend = "\u001b[0m"
    bullet = "\u2022"
  } else {
    boldstart = ""
    formatend = ""
    bullet = "*"
  }
  generate_text = function(label, output) {
    sprintf("%s%s %s = %s%s ",boldstart, bullet, label, formatend, output)
  }

  alphatext = generate_text("Alpha", as.character(attr(x,"alpha")))
  trialtext = generate_text("Trials", nrow(attr(x,"runmatrix")))

  blocking = FALSE
  if(!is.null(attr(x,"blocking"))) {
    blocking = attr(x,"blocking")
  }
  if(!is.null(attr(x,"splitplot"))) {
    blocking = blocking || attr(x,"splitplot")
  }
  x2 = rbind(as.matrix(x),colnames(x))
  row_width_char = max(nchar(rownames(x)))
  x2[is.na(x2)] = "NA"
  total_width = sum(apply(x2,2,(function(x) max(nchar(x))))) + row_width_char + length(colnames(x))
  blocktext = generate_text("Blocked", as.character(blocking))
  totalline = paste0(alphatext,trialtext,blocktext)
  designinfo = "Evaluation Info"
  linewidth = total_width
  if (linewidth > nchar(designinfo)) {
    difference = linewidth-nchar(designinfo)
    if (difference %% 2 == 0) {
      firstspacer = difference / 2
      secondspacer = difference / 2
    } else {
      firstspacer = (difference - 1 ) / 2
      secondspacer = (difference - 1) / 2 + 1
    }
    titlex = paste0(c(paste0(rep("=", firstspacer), collapse = ""),
                    designinfo,
                    paste0(rep("=", secondspacer), collapse = "")), collapse = "")
    cat(titlex, sep = "\n")
  } else {
    cat(paste0(rep("=", linewidth), collapse=""), sep = "\n")
  }
  cat(paste0(totalline, collapse=""), sep = "\n")
  cat(generate_text("Evaluating Model", paste(as.character(attr(x,"generating.model")), collapse="")), sep = "\n")
  if(all(attr(x,"anticoef") %in% c(-1, 0,1))) {
    anticoef_str = sprintf("c(%s)",paste0(unlist(lapply("%1.0f",sprintf, attr(x,"anticoef"))), collapse=", "))
    cat(generate_text("Anticipated Coefficients",anticoef_str), sep = "\n")
  } else {
    anticoef_str = sprintf("c(%s)",paste0(unlist(lapply("%1.3f",sprintf, attr(x,"anticoef"))), collapse=", "))
    cat(generate_text("Anticipated Coefficients",anticoef_str), sep = "\n")
  }
  if (!is.null(attr(x,"z.matrix.list")) && blocking) {
    number_blocks = unlist(lapply(attr(x,"z.matrix.list"),ncol))
    block_str = paste(paste("Level ", 1:length(number_blocks), ": ", number_blocks, sep = ""),
                      collapse=", ")
    cat(generate_text("Number of Blocks",block_str), sep = "\n")
  }
  if (!is.null(attr(x,"varianceratios")) && blocking) {
    vr = attr(x,"varianceratios")
    vr_str =  paste(paste("Level ", 1:length(vr), ": ",as.character(vr), sep="", collapse=", "))
    cat(generate_text("Variance Ratios ",vr_str), sep = "\n")
  }
  if(!is.null(attr(x, "contrast_string"))) {
    cat(generate_text("Contrasts", attr(x, "contrast_string")),sep="\n")
  }
  if(!is.null(attr(x, "parameter_analysis_method_string"))) {
    if(nchar(attr(x, "parameter_analysis_method_string")) > 0) {
      cat(generate_text("Parameter Analysis Method", attr(x, "parameter_analysis_method_string")),sep="\n")
    }
  }
  if(!is.null(attr(x, "effect_analysis_method_string"))) {
    if(nchar(attr(x, "effect_analysis_method_string")) > 0) {
      cat(generate_text("Effect Analysis Method", attr(x, "effect_analysis_method_string")),sep="\n")
    }
  }
}

#'@title Print evaluation information
#'
#'@description Prints design evaluation information below the data.frame of power values
#'
#'Note: If options("skpr.ANSI") is `NULL` or `TRUE`, ANSI codes will be used during printing
#'to prettify the output. If this is `FALSE`, only ASCII will be used.
#'
#'@param x The x of the evaluation functions in skpr
#'@param ... Additional arguments.
#'@import graphics grDevices
#'@export
#'@examples
#'#Generate/evaluate a design and print its information
#'factorialcoffee = expand.grid(cost = c(1, 2),
#'                               type = as.factor(c("Kona",
#'                                                  "Colombian",
#'                                                  "Ethiopian",
#'                                                  "Sumatra")),
#'                               size = as.factor(c("Short",
#'                                                  "Grande",
#'                                                  "Venti")))
#'
#'coffee_curves = calculate_power_curves(candidateset = factorialcoffee,
#'                                       model = ~(cost + size + type)^2,
#'                                       trials = 30:40, plot_results = FALSE)
#'coffee_curves
print.skpr_power_curve_output = function(x, ...) {
  curve_warn_error = attr(x, "output")
  class(x) = "data.frame"
  print(x)
  if((is.null(getOption("skpr.ANSI")) || getOption("skpr.ANSI")) && !is_rendering_in_knitr()) {
    boldstart = "\u001b[1m"
    formatend = "\u001b[0m"
  } else {
    boldstart = ""
    formatend = ""
  }
  if(!(length(find.package("cli", quiet = TRUE)) > 0)) {
    warning("{cli} package required for color annotations on output")
    col_red = col_blue = col_magenta = col_green = function(x) x

  } else {
    col_red = cli::col_red
    col_blue = cli::col_blue
    col_magenta = cli::col_magenta
    col_green = cli::col_green
  }
  curve_warn_error$gen_errors    = unique(curve_warn_error$gen_errors[curve_warn_error$gen_errors$err        != "",])
  curve_warn_error$gen_warnings  = unique(curve_warn_error$gen_warnings[curve_warn_error$gen_warnings$warn   != "",])
  curve_warn_error$eval_errors   = unique(curve_warn_error$eval_errors[curve_warn_error$eval_errors$err      != "",])
  curve_warn_error$eval_warnings = unique(curve_warn_error$eval_warnings[curve_warn_error$eval_warnings$warn != "",])


  unique_gen_errors    = table(curve_warn_error$gen_errors$err)
  unique_gen_warnings  = table(curve_warn_error$gen_warnings$warn)
  unique_eval_errors   = table(curve_warn_error$eval_errors$err)
  unique_eval_warnings = table(curve_warn_error$eval_warnings$warn)
  unique_vals = c(unique_gen_errors,unique_gen_warnings,unique_eval_errors,unique_eval_warnings)
  if(length(unique_vals) > 0) {
    max_num = max(unique_vals,na.rm=TRUE)
    max_width = max(c(ceiling(log10(max_num)),1))
    cat(sprintf(col_red("%sPower curve generation captured the following warning/error messages:%s"),boldstart,formatend),
        sep="\n")
    cat(sprintf(col_blue("%s%0.*s | %.4s | %.*s%s | Message%s"),
                boldstart,
                10,
                "Function        ",
                "Type     ",
                max_width, "N", paste0(rep(" ", max_width-1),collapse=""),
                formatend), sep = "\n")
    if(length(unique_gen_errors) > 0) {
      msg_val = names(unique_gen_errors)
      names(unique_gen_errors) = NULL
      for(i in seq_len(length(msg_val))) {
        pad = max_width-nchar(as.character(unique_gen_errors[i]))
        pad_str = ifelse(pad <= 0, "", paste0(rep(" ", pad),collapse=""))
        cat(sprintf("%s%.10s | %.4s | %.*s%s | Message:%s '%s'",
                    boldstart,
                    "Generation",
                    "Err ",
                    max_width,
                    as.character(unique_gen_errors[i]),
                    pad_str,
                    formatend, col_red(msg_val[i])),sep = "\n")
      }
    }
    if(length(unique_gen_warnings) > 0) {
      msg_val = names(unique_gen_warnings)
      names(unique_gen_warnings) = NULL
      for(i in seq_len(length(msg_val))) {
        pad = max_width-nchar(as.character(unique_gen_warnings[i]))
        pad_str = ifelse(pad <= 0, "", paste0(rep(" ", pad),collapse=""))
        cat(sprintf("%s%.10s | %.4s | %.*s%s | Message:%s '%s'",
                    boldstart,
                    "Generation",
                    "Warn",
                    max_width,
                    as.character(unique_gen_warnings[i]),
                    pad_str,
                    formatend, col_magenta(msg_val[i])),sep = "\n")
      }
    }
    if(length(unique_eval_errors) > 0) {
      msg_val = names(unique_eval_errors)
      names(unique_eval_errors) = NULL
      for(i in seq_len(length(msg_val))) {
        pad = max_width-nchar(as.character(unique_eval_errors[i]))
        pad_str = ifelse(pad <= 0, "", paste0(rep(" ", pad),collapse=""))
        cat(sprintf("%s%.10s | %.4s | %.*s%s | Message:%s '%s'",
                    boldstart,
                    "Evaluation",
                    "Err ",
                    max_width,
                    as.character(unique_eval_errors[i]),
                    pad_str,
                    formatend, col_red(msg_val[i])),sep = "\n")
      }
    }
    if(length(unique_eval_warnings) > 0) {
      msg_val = names(unique_eval_warnings)
      names(unique_eval_warnings) = NULL
      for(i in seq_len(length(msg_val))) {
        pad = max_width-nchar(as.character(unique_eval_warnings[i]))
        pad_str = ifelse(pad <= 0, "", paste0(rep(" ", pad),collapse=""))
        cat(sprintf("%s%.10s | %.4s | %.*s%s | Message:%s '%s'",
                    boldstart,
                    "Evaluation",
                    "Warn",
                    max_width,
                    as.character(unique_eval_warnings[i]),
                    pad_str,
                    formatend, col_magenta(msg_val[i])),sep = "\n")
      }
    }
  } else {
    cat(sprintf(col_green("%sNo errors or warnings encountered during power curve generation!%s"),boldstart,formatend),
        sep="\n")
  }
}
