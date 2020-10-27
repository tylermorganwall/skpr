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
  if(is.null(getOption("skpr.ANSI")) || getOption("skpr.ANSI")) {
    boldstart = "\u001b[1m"
    formatend = "\u001b[0m"
  } else {
    boldstart = ""
    formatend = ""
  }
  alphatext = paste(c(boldstart, "\u2022 Alpha = ",formatend, as.character(attr(x,"alpha")), " "), collapse = "")
  trialtext = paste(c(boldstart, "\u2022 Trials = ",formatend, nrow(attr(x,"runmatrix")), " "), collapse = "")
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
  blocktext = paste(c(boldstart, "\u2022 Blocked = ",formatend, as.character(blocking)), collapse = "")
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
    cat(paste0(titlex, "\n", collapse = ""))

  } else {
    cat(paste0(rep("=", linewidth), "\n", collapse=""))
  }
  cat(paste0(totalline, collapse=""))
  cat("\n")
  cat(paste(c(boldstart, "\u2022 Evaluating Model = ",formatend, as.character(attr(x,"generating.model")),"\n"), collapse = ""))
  if(all(abs(attr(x,"anticoef")) %in% c(0,1))) {
    cat(paste(c(boldstart, "\u2022 Anticipated Coefficients = ",formatend, "c(",
                paste0(unlist(lapply("%1.0f",sprintf, attr(x,"anticoef"))),collapse=", "),")\n"), collapse = ""))
  } else {
    cat(paste(c(boldstart, "\u2022 Anticipated Coefficients = ",formatend, "c(",
                paste0(unlist(lapply("%1.3f",sprintf, attr(x,"anticoef"))),collapse=", "),")\n"), collapse = ""))
  }
  if (!is.null(attr(x,"z.matrix.list")) && blocking) {
    number_blocks = unlist(lapply(attr(x,"z.matrix.list"),ncol))
    cat(paste(c(boldstart, "\u2022 Number of Blocks = ",formatend,
                paste(paste("Level ", 1:length(number_blocks), ": ", number_blocks, sep = ""),
                      collapse=", "),"\n"),
                collapse = ""))
  }
  if (!is.null(attr(x,"varianceratios")) && blocking) {
    vr = attr(x,"varianceratios")
    # vr = vr[-length(vr)]
    cat(paste(c(boldstart, "\u2022 Variance Ratios  = ",formatend,
                paste(paste("Level ", 1:length(vr), ": ",as.character(vr), sep="", collapse=", ")),"\n"),
              collapse = ""))
  }
}
