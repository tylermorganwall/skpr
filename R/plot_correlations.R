#'@title Plots design diagnostics
#'
#'@description Plots design diagnostics
#'
#'@param skpr_output The output of either [gen_design()], [eval_design()], or [eval_design_mc()].
#'@param model Default `NULL`. Defaults to the model used in generating/evaluating
#'the design, augmented with 2-factor interactions. If specified, it will override the default
#'model used to generate/evaluate the design.
#'@param customcolors A vector of colors for customizing the appearance of the colormap
#'@param pow Default 2. The interaction level that the correlation map is showing.
#'@param custompar Default NULL. Custom parameters to pass to the `par` function for base R plotting.
#'@param standardize Default `TRUE`. Whether to standardize (scale to -1 and 1 and center) the continuous numeric columns. Not
#'standardizing the numeric columns can increase multi-collinearity (predictors that are correlated with other predictors in the model).
#'@param plot Default `TRUE`. If `FALSE`, this will return the correlation matrix.
#'@return Silently returns the correlation matrix with the proper row and column names.
#'@import graphics grDevices
#'@export
#'@examples
#'#We can pass either the output of gen_design or eval_design to plot_correlations
#'#in order to obtain the correlation map. Passing the output of eval_design is useful
#'#if you want to plot the correlation map from an externally generated design.
#'
#'#First generate the design:
#'
#'candidatelist = expand.grid(cost = c(15000, 20000), year = c("2001", "2002", "2003", "2004"),
#'                            type = c("SUV", "Sedan", "Hybrid"))
#'cardesign = gen_design(candidatelist, ~(cost+type+year)^2, 30)
#'plot_correlations(cardesign)
#'
#'#We can also increase the level of interactions that are shown by default.
#'
#'plot_correlations(cardesign, pow = 3)
#'
#'#You can also pass in a custom color map.
#'plot_correlations(cardesign, customcolors = c("blue", "grey", "red"))
#'plot_correlations(cardesign, customcolors = c("blue", "green", "yellow", "orange", "red"))
plot_correlations = function(
  skpr_output,
  model = NULL,
  customcolors = NULL,
  pow = 2,
  custompar = NULL,
  standardize = TRUE,
  plot = TRUE
) {
  #Remove skpr-generated REML blocking indicators if present
  if (!is.null(attr(skpr_output, "splitanalyzable"))) {
    if (attr(skpr_output, "splitanalyzable")) {
      allattr = attributes(skpr_output)
      remove_cols = which(colnames(skpr_output) %in% allattr$splitcolumns)
      if (length(remove_cols) > 0) {
        skpr_output = skpr_output[, -remove_cols, drop = FALSE]
        allattr$names = allattr$names[-remove_cols]
      }
      attributes(skpr_output) = allattr
    }
  }
  if (!is.null(attr(skpr_output, "splitcolumns"))) {
    allattr = attributes(skpr_output)
    skpr_output = skpr_output[,
      !(colnames(skpr_output) %in% attr(skpr_output, "splitcolumns")),
      drop = FALSE
    ]
    allattr$names = allattr$names[
      !allattr$names %in% attr(skpr_output, "splitcolumns")
    ]
    attributes(skpr_output) = allattr
  }
  if (!is.null(attr(skpr_output, "augmented"))) {
    if (attr(skpr_output, "augmented")) {
      allattr = attributes(skpr_output)
      skpr_output = skpr_output[,
        !(colnames(skpr_output) %in% "Block1"),
        drop = FALSE
      ]
      allattr$names = allattr$names[!allattr$names %in% "Block1"]
      attributes(skpr_output) = allattr
    }
  }
  if (is.null(attr(skpr_output, "variance.matrix"))) {
    skpr_output = eval_design(skpr_output, model, 0.2)
  }
  V = attr(skpr_output, "variance.matrix")
  #Maybe add candidate set to the attributes,
  if (!is.null(attr(skpr_output, "runmatrix"))) {
    design = attr(skpr_output, "runmatrix")
  } else {
    design = skpr_output
  }
  if (is.null(model)) {
    variables = paste0("`", colnames(design), "`")
    linearterms = paste(variables, collapse = " + ")
    linearmodel = paste0(c("~", linearterms), collapse = "")
    model1 = as.formula(paste(
      c(
        linearmodel,
        as.character(aliasmodel(as.formula(linearmodel), power = pow)[2])
      ),
      collapse = " + "
    ))
    if (!is.null(attr(skpr_output, "generating_model"))) {
      modelfactors = colnames(attr(
        terms.formula(attr(skpr_output, "generating_model"), data = design),
        "factors"
      ))
      quadmodelfactors = colnames(attr(
        terms.formula(model1, "factors", data = design),
        "factors"
      ))
      otherterms = modelfactors[!modelfactors %in% quadmodelfactors]
      model = as.formula(paste(c(model1, otherterms), collapse = " + "))
    } else {
      model = model1
    }
  } else {
    model1 = model
  }

  factornames = colnames(design)[
    unlist(lapply(design, class)) %in% c("factor", "character")
  ]
  if (length(factornames) > 0) {
    contrastlist = list()
    for (name in 1:length(factornames)) {
      contrastlist[[factornames[name]]] = contr.simplex
    }
  } else {
    contrastlist = NULL
  }
  #------Normalize/Center numeric columns ------#
  if (standardize) {
    for (column in seq_len(ncol(skpr_output))) {
      if (is.numeric(design[, column])) {
        midvalue = mean(c(max(design[, column]), min(design[, column])))
        design[, column] = (design[, column] - midvalue) /
          (max(design[, column]) - midvalue)
      }
    }
  }

  mm_main = model.matrix(model, design, contrasts.arg = contrastlist)
  mm = model.matrix(model1, design, contrasts.arg = contrastlist)
  X = mm_main[, -1, drop = FALSE]
  int_nms = setdiff(colnames(mm_main), colnames(mm)) # just the interactions
  Z = mm[, int_nms, drop = FALSE]
  W = solve(V)

  XZ = cbind(X, Z)
  G = crossprod(XZ, W %*% XZ)
  C = cov2cor(G)
  cormat = abs(C)
  if (!plot) {
    return(cormat)
  }

  if (is.null(customcolors)) {
    imagecolors = viridis::viridis(101)
  } else {
    imagecolors = colorRampPalette(customcolors)(101)
  }
  if (is.null(custompar)) {
    par(mar = c(5, 3, 7, 0))
  } else {
    do.call(par, custompar)
  }
  image(
    t(cormat[ncol(cormat):1, , drop = FALSE]),
    x = 1:ncol(cormat),
    y = 1:ncol(cormat),
    zlim = c(0, 1),
    asp = 1,
    axes = F,
    col = imagecolors,
    xlab = "",
    ylab = ""
  )
  axis(
    3,
    at = 1:ncol(cormat),
    labels = colnames(mm)[-1],
    pos = ncol(cormat) + 1,
    las = 2,
    hadj = 0,
    cex.axis = 0.8
  )
  axis(
    2,
    at = ncol(cormat):1,
    labels = colnames(mm)[-1],
    pos = 0,
    las = 2,
    hadj = 1,
    cex.axis = 0.8
  )

  legend(
    length(colnames(mm)[-1]) + 1,
    length(colnames(mm)[-1]),
    c("0", "", "", "", "", "0.5", "", "", "", "", "1.0"),
    title = "|r|\n",
    fill = imagecolors[c(seq(1, 101, 10))],
    xpd = TRUE,
    bty = "n",
    border = NA,
    y.intersp = 0.3,
    x.intersp = 0.1,
    cex = 1
  )
  par(mar = c(5.1, 4.1, 4.1, 2.1))

  results = t(cormat[ncol(cormat):1, , drop = FALSE])
  colnames(results) = rev(colnames(mm)[-1])
  rownames(results) = colnames(mm)[-1]
  invisible(results)
}
