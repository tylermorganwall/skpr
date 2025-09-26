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
  presetcontrasts = list()
  contrast_info = generate_contrast_list(
    design,
    presetcontrasts,
    contr.simplex
  )
  contrastslist_cormat = contrast_info$contrastslist_cormat
  #------Normalize/Center numeric columns ------#
  if (standardize) {
    design = normalize_design(design)
  }
  #Main effects model
  mm_main = model.matrix(~., design, contrasts.arg = contrastslist_cormat)
  #All interactions included
  mm = model.matrix(model1, design, contrasts.arg = contrastslist_cormat)

  X = mm_main[, -1, drop = FALSE]
  int_nms = setdiff(colnames(mm), colnames(mm_main)) # just the interactions

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
  if (!is.null(custompar)) {
    warning(
      "`custompar` is no longer supported; adjust the returned ggplot object instead."
    )
  }
  labels = colnames(mm)[-1]
  plot_matrix = t(cormat[ncol(cormat):1, , drop = FALSE])
  plot_df = data.frame(
    x = rep(labels, each = length(labels)),
    y = rep(rev(labels), times = length(labels)),
    value = as.vector(plot_matrix)
  )
  plot_obj = ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = factor(x, levels = labels),
      y = factor(y, levels = rev(labels)),
      fill = value
    )
  ) +
    ggplot2::geom_tile(color = NA) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete() +
    ggplot2::scale_fill_gradientn(
      colours = imagecolors,
      limits = c(0, 1),
      name = "|r|"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 0,
        size = 8
      ),
      axis.text.y = ggplot2::element_text(size = 8)
    )
  print(plot_obj)

  results = t(cormat[ncol(cormat):1, , drop = FALSE])
  colnames(results) = rev(colnames(mm)[-1])
  rownames(results) = colnames(mm)[-1]
  invisible(results)
}
