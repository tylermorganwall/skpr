#'@title Fraction of Design Space Plot
#'
#'@description Creates a fraction of design space plot
#'
#'@param skpr_output The design, or the output of the power evaluation functions. This can also be a list
#'of several designs, which will result in all of them being plotted in a row (for easy comparison).
#'@param model Default `NULL`. The model, if `NULL` it defaults to the model used in [eval_design()] or [gen_design()].
#'@param continuouslength Default `11`. The precision of the continuous variables. Decrease for faster (but less precise) plotting.
#'@param plot Default `TRUE`. Whether to plot the FDS, or just calculate the cumulative distribution function.
#'@param sample_size Default `10000`. Number of samples to take of the design space.
#'@param yaxis_max Default `NULL`. Manually set the maximum value of the prediction variance.
#'@param description Default `Fraction of Design Space`. The description to add to the plot. If a vector and multiple designs
#'passed to skpr_output, it will be the description for each plot.
#'@param moment_sample_density Default `10`. The density of points to sample when calculating the moment matrix to
#' compute I-optimality if there are disallowed combinations. Otherwise, the closed-form moment matrix can be calculated.
#'@param candidate_set Default `NA`. If the original design did not come from skpr and has disallowed combinations, the average prediction variance
#' over the design region needs the original candidate set to accurately compute the I-optimality value. Note that this will estimate the valid design region
#' using the convex hull of the given points, which is slow computationally for large designs: pass a `high_resolution_candidate_set` for faster plotting.
#'@param high_resolution_candidate_set Default `NA`. If you have continuous numeric terms and disallowed combinations, the closed-form I-optimality value
#' cannot be calculated and must be approximated by numeric integration. This requires sampling the allowed space densely, but most candidate sets will provide
#' a sparse sampling of allowable points. To work around this, skpr will generate a convex hull of the numeric terms for each unique combination of categorical
#' factors to generate a dense sampling of the space and cache that value internally, but this is a slow calculation and does not support non-convex candidate sets.
#' To speed up moment matrix calculation,  pass a higher resolution version of your candidate set here with the disallowed combinations already applied.
#'@return Plots design diagnostics, and invisibly returns the vector of values representing the fraction of design space plot. If multiple
#'designs are passed, this will return a list of all FDS vectors.
#'@import graphics grDevices
#'@export
#'@examples
#'#We can pass either the output of gen_design or eval_design to plot_correlations
#'#in order to obtain the correlation map. Passing the output of eval_design is useful
#'#if you want to plot the correlation map from an externally generated design.
#'
#'#First generate the design:
#'
#'candidate_set = expand.grid(X1 = c(1, -1), X2 = c(1, -1))
#'
#'design = gen_design(candidate_set, ~(X1 + X2), 15)
#'
#'plot_fds(design)
#'
#'#We can also feed evaluation output
#'power = eval_design(design)
#'plot_fds(power)
plot_fds = function(
  skpr_output,
  model = NULL,
  continuouslength = 1001,
  plot = TRUE,
  sample_size = 10000,
  yaxis_max = NULL,
  moment_sample_density = 10,
  description = "Fraction of Design Space",
  candidate_set = NA,
  high_resolution_candidate_set = NA
) {
  if (inherits(skpr_output, "list") && length(skpr_output) > 1) {
    fds_values = vector("list", length(skpr_output))
    if (!plot && !is.null(yaxis_max)) {
      warning(
        "`plot = FALSE` but `yaxis_max` non-NULL. Setting `yaxis_max` to NULL"
      )
      yaxis_max = NULL
    }
    for (i in seq_along(skpr_output)) {
      fds_values[[i]] = plot_fds(
        skpr_output[[i]],
        model = model,
        continuouslength = continuouslength,
        plot = FALSE,
        sample_size = sample_size,
        moment_sample_density = moment_sample_density,
        candidate_set = candidate_set,
        high_resolution_candidate_set = high_resolution_candidate_set
      )
    }
    if (is.null(yaxis_max)) {
      flattened = unlist(fds_values, use.names = FALSE)
      yaxis_max = max(flattened) + max(flattened) / 20
    }
    if (length(description) == 1) {
      description = rep(description, length(skpr_output))
    }
    if (plot) {
      mid_index = function(values) {
        max(1, min(length(values), round(sample_size / 2)))
      }
      plot_list = vector("list", length(skpr_output))
      for (i in seq_along(skpr_output)) {
        values = fds_values[[i]]
        midval = values[mid_index(values)]
        fraction = seq_along(values) / length(values)
        maxval = max(values)
        df = data.frame(
          fraction = fraction,
          variance = values
        )
        plot_list[[i]] = ggplot2::ggplot(
          df,
          ggplot2::aes(x = fraction, y = variance)
        ) +
          ggplot2::geom_line(color = "blue", linewidth = 1) +
          ggplot2::geom_vline(
            xintercept = 0.5,
            linetype = "dashed",
            color = "red"
          ) +
          ggplot2::geom_hline(
            yintercept = midval,
            linetype = "dashed",
            color = "red"
          ) +
          ggplot2::geom_hline(
            yintercept = maxval,
            linetype = "dashed",
            color = "black"
          ) +
          ggplot2::annotate(
            "text",
            label = sprintf("Mid PV: %0.3f", midval),
            x = 0,
            hjust = 0.0,
            y = midval,
            vjust = -0.25,
            fontface = "bold",
            size = 6,
            color = "red"
          ) +
          ggplot2::annotate(
            "text",
            label = sprintf("Max PV: %0.3f", maxval),
            x = 0,
            hjust = 0.0,
            y = maxval,
            vjust = -0.25,
            size = 6,
            color = "black",
            fontface = "bold",
          ) +
          ggplot2::scale_x_continuous(
            limits = c(0, 1),
            expand = c(0, 0.0)
          ) +
          ggplot2::scale_y_continuous(
            limits = c(0, maxyaxis),
            expand = ggplot2::expansion(mult = 0)
          ) +
          ggplot2::labs(
            x = description,
            y = "Prediction Variance"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(margins = ggplot2::unit(c(20, 20, 20, 20), "points")) +
          ggplot2::theme(text = ggplot2::element_text(size = 24)) +
          ggplot2::coord_cartesian(clip = F)
      }
      if (requireNamespace("gridExtra", quietly = TRUE)) {
        do.call(
          gridExtra::grid.arrange,
          c(plot_list, list(nrow = 1))
        )
      } else {
        warning(
          "Package `gridExtra` not available; displaying plots sequentially."
        )
        lapply(plot_list, print)
      }
    }
    return(invisible(fds_values))
  }
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
  if (!is.null(attr(skpr_output, "runmatrix"))) {
    design = attr(skpr_output, "runmatrix")
  } else {
    design = skpr_output
  }
  new_model = FALSE
  factor_levels = names(design)[
    lapply(design, class) %in%
      c("character", "factor")
  ]
  temp_contrasts_list =
    setNames(rep(list(contr.simplex), length(factor_levels)), factor_levels)
  if (is.null(model)) {
    if (!is.null(attr(skpr_output, "generating_model"))) {
      model = attr(skpr_output, "generating_model")
      model_matrix = attr(skpr_output, "model_matrix")
      factors = colnames(model_matrix)
    } else {
      model = ~.
      model_matrix = model.matrix(
        model,
        data = design,
        contrasts.arg = temp_contrasts_list
      )
      factors = colnames(model_matrix)
    }
  } else {
    new_model = TRUE
    model_matrix = model.matrix(
      model,
      data = design,
      contrasts.arg = temp_contrasts_list
    )
    factors = colnames(model_matrix)
  }
  #Need these inputs
  if (!is.null(attr(skpr_output, "runmatrix"))) {
    design = attr(skpr_output, "runmatrix")
  } else {
    design = skpr_output
  }
  #detect pre-set contrasts
  presetcontrasts = list()
  for (x in names(design)[
    lapply(design, class) %in% c("character", "factor")
  ]) {
    if (!is.null(attr(design[[x]], "contrasts"))) {
      presetcontrasts[[x]] = attr(design[[x]], "contrasts")
    }
  }
  #---Develop contrast lists for model matrix---#
  #Variables used later: contrastslist, contrastslist_cormat
  contrast_info = generate_contrast_list(
    design,
    presetcontrasts,
    contrasts
  )
  contrastslist = contrast_info$contrastslist
  contrastslist_cormat = contrast_info$contrastslist_cormat
  classvector = sapply(lapply(design, unique), class) == "factor"

  if (is.na(candidate_set)) {
    candidate_set = attr(skpr_output, "candidate_set")
  }
  if (is.null(candidate_set)) {
    stop(
      "If design was not originally generated with skpr, you must provide a `candidate_set` to compute fraction of design space plots."
    )
  }
  normalized_candidate_set = normalize_design(candidate_set)

  candset_verts = candidate_set
  for (i in seq_len(ncol(candset_verts))) {
    if (is.numeric(candset_verts[[i]])) {
      num_col = candset_verts[[i]]
      min_max = range(candset_verts[[i]])
      is_exterior = num_col %in% min_max
      candset_verts = candset_verts[is_exterior, , drop = FALSE]
    }
  }
  contrastslist = attr(design, "contrastslist")
  candset_mm = model.matrix(
    model,
    data = candset_verts,
    contrasts = contrastslist
  )
  invXtX = solve(t(model_matrix) %*% model_matrix)

  spv_verts = diag(candset_mm %*% invXtX %*% t(candset_mm))

  #Need to generate this here
  Iopt = attr(skpr_output, "I")
  if (is.na(Iopt)) {
    moment_matrix = get_moment_matrix(
      design = design,
      candidate_set_normalized = normalized_candidate_set,
      factors = factors,
      classvector = classvector,
      model = model,
      moment_sample_density = moment_sample_density,
      high_resolution_candidate_set = high_resolution_candidate_set
    )
    if (!new_model) {
      model_matrix_cor = attr(skpr_output, "model_matrix_cor")
    } else {
      model_matrix_cor = model.matrix(
        model,
        data = design,
        contrasts.arg = temp_contrasts_list
      )
    }
    Iopt = calculate_i_optimality_safe(
      model_matrix_cor,
      moment_matrix,
      diag(nrow(model_matrix_cor))
    )
  }
  if (is.na(Iopt) || is.null(Iopt)) {
    stop(
      "No I-optimality value found in design--was your design generated outside of skpr? If so, pass in a high resolution candidate set to `high_resolution_candidate_set` to ensure I-optimality is computed."
    )
  }
  V = attr(skpr_output, "variance.matrix")

  if (!is.null(attr(skpr_output, "runmatrix"))) {
    skpr_output = attr(skpr_output, "runmatrix")
  }

  factornames = colnames(skpr_output)[
    unlist(lapply(skpr_output, class)) %in% c("factor", "character")
  ]
  if (length(factornames) > 0) {
    contrastlist = list()
    for (name in 1:length(factornames)) {
      contrastlist[[factornames[name]]] = "contr.sum"
    }
  } else {
    contrastlist = NULL
  }
  sample_list = list()

  for (col in seq_len(ncol(skpr_output))) {
    if (inherits(skpr_output[, col], c("factor", "character"))) {
      vals = unique(skpr_output[, col])
    }
    if (is.numeric(skpr_output[, col])) {
      vals = seq(-1, 1, length.out = continuouslength)
    }
    sample_list[[colnames(skpr_output)[col]]] = vals[sample(
      seq_len(length(vals)),
      size = sample_size,
      replace = TRUE
    )]
  }
  samples = as.data.frame(sample_list)

  #------Normalize/Center numeric columns ------#
  skpr_output_norm = normalize_design(skpr_output)
  mm = model.matrix(model, skpr_output_norm, contrasts.arg = contrastlist)
  samplemm = model.matrix(model, samples, contrasts.arg = contrastlist)

  testcor = solve(t(mm) %*% solve(V) %*% mm)

  vals_interior = diag(samplemm %*% testcor %*% t(samplemm))

  vars = c(spv_verts, vals_interior)
  varsordered = vars[order(vars)]
  meanindex = which(
    abs(mean(varsordered) - varsordered) ==
      min(abs(mean(varsordered) - varsordered))
  )

  scale = varsordered[meanindex]
  if (length(scale) > 1) {
    scale = scale[1]
  }
  varsorderedscaled = varsordered / scale * Iopt
  midval = varsorderedscaled[sample_size / 2]
  if (is.null(yaxis_max)) {
    maxyaxis = max(varsorderedscaled) + max(varsorderedscaled) / 20
  } else {
    maxyaxis = yaxis_max
  }
  if (plot) {
    mid_index = max(1, min(length(varsorderedscaled), round(sample_size / 2)))
    midval = varsorderedscaled[mid_index]
    df = data.frame(
      fraction = seq_along(varsorderedscaled) / length(varsorderedscaled),
      variance = varsorderedscaled
    )
    maxval = max(varsorderedscaled)
    plot_obj = ggplot2::ggplot(df, ggplot2::aes(x = fraction, y = variance)) +
      ggplot2::geom_line(color = "blue", linewidth = 1) +
      ggplot2::geom_vline(
        xintercept = 0.5,
        linetype = "dashed",
        color = "red"
      ) +
      ggplot2::geom_hline(
        yintercept = midval,
        linetype = "dashed",
        color = "red"
      ) +
      ggplot2::geom_hline(
        yintercept = maxval,
        linetype = "dashed",
        color = "black"
      ) +
      ggplot2::annotate(
        "text",
        label = sprintf("Mid PV: %0.3f", midval),
        x = 0,
        hjust = 0.0,
        y = midval,
        vjust = -0.25,
        fontface = "bold",
        size = 6,
        color = "red"
      ) +
      ggplot2::annotate(
        "text",
        label = sprintf("Max PV: %0.3f", maxval),
        x = 0,
        hjust = 0.0,
        y = maxval,
        vjust = -0.25,
        size = 6,
        color = "black",
        fontface = "bold",
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        expand = c(0, 0.0)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, maxyaxis),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::labs(
        x = description,
        y = "Prediction Variance"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(margins = ggplot2::unit(c(20, 20, 20, 20), "points")) +
      ggplot2::theme(text = ggplot2::element_text(size = 24)) +
      ggplot2::coord_cartesian(clip = F)
    print(plot_obj)
    return(invisible(varsorderedscaled))
  }
  return(varsorderedscaled)
}
globalVariables(c("variance"))
