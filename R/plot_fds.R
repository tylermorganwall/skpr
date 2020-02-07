#'@title Fraction of Design Space Plot
#'
#'@description Creates a fraction of design space plot
#'
#'@param genoutput The design, or the output of the power evaluation functions. This can also be a list
#'of several designs, which will result in all of them being plotted in a row (for easy comparison).
#'@param model Default `NULL`. The model, if `NULL` it defaults to the model used in `eval_design` or `gen_design`.
#'@param continuouslength Default `31`. The precision of the continuous variables. Decrease for faster (but less precise) plotting.
#'@param plot Default `TRUE`. Whether to plot the FDS, or just calculate the cumulative distribution function.
#'@param yaxis_max Default `NULL`. Manually set the maximum value of the prediction variance.
#'@param description Default `Fraction of Design Space`. The description to add to the plot.
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
#'candidatelist = expand.grid(X1 = c(1, -1), X2 = c(1, -1))
#'
#'design = gen_design(candidatelist, ~(X1 + X2), 15)
#'
#'plot_fds(design)
plot_fds = function(genoutput, model = NULL, continuouslength = 31, plot=TRUE,
                    yaxis_max = NULL, description="Fraction of Design Space") {
  if(inherits(genoutput,"list") && length(genoutput) > 1) {
    old.par = par(no.readonly = TRUE)
    on.exit(par(old.par))
    par(mfrow = c(1,length(genoutput)))
    fds_values = list()
    if(!plot && !is.null(yaxis_max)) {
      warning("`plot = FALSE` but `yaxis_max` non-NULL. Setting `yaxis_max` to NULL")
      yaxis_max = NULL
    }
    if(is.null(yaxis_max)) {
      for(i in 1:length(genoutput)) {
        fds_values[[i]] = plot_fds(genoutput[[i]], model=model,
                                   continuouslength = continuouslength, plot=FALSE)
      }
      yaxis_max = max(unlist(fds_values)) + max(unlist(fds_values)) / 20
    }
    if(plot) {
      for(i in 1:length(genoutput)) {
        fds_values[[i]] = plot_fds(genoutput[[i]], model=model, continuouslength = continuouslength,
                                   plot=plot, yaxis_max=yaxis_max,
                                   description = paste0(c(description, "\nDesign ",i),collapse=""))
      }
    }
    return(invisible(fds_values))
  }
  #Remove skpr-generated REML blocking indicators if present
  if (!is.null(attr(genoutput, "splitanalyzable"))) {
    if (attr(genoutput, "splitanalyzable")) {
      allattr = attributes(genoutput)
      genoutput = genoutput[, -1:-length(allattr$splitcolumns)]
      allattr$names = allattr$names[-1:-length(allattr$splitcolumns)]
      attributes(genoutput) = allattr
    }
  }
  if (!is.null(attr(genoutput, "splitcolumns"))) {
    if(attr(genoutput, "blocking")) {
      allattr = attributes(genoutput)
      genoutput = genoutput[, !(colnames(genoutput) %in% attr(genoutput, "splitcolumns")), drop = FALSE]
      allattr$names = allattr$names[-1:-length(allattr$splitcolumns)]
      attributes(genoutput) = allattr
    }
  }

  Iopt = attr(genoutput, "I")
  V = attr(genoutput, "variance.matrix")

  if(is.null(model)) {
    if(!is.null(attr(genoutput, "generating.model"))) {
      model = attr(genoutput, "generating.model")
    } else {
      model = ~.
    }
  }
  if (!is.null(attr(genoutput, "run.matrix"))) {
    genoutput = attr(genoutput, "run.matrix")
  }

  factornames = colnames(genoutput)[unlist(lapply(genoutput, class)) %in% c("factor", "character")]
  if (length(factornames) > 0) {
    contrastlist = list()
    for (name in 1:length(factornames)) {
      contrastlist[[factornames[name]]] = "contr.sum"
    }
  } else {
    contrastlist = NULL
  }
  factorrange = list()
  for (col in 1:ncol(genoutput)) {
    if (class(genoutput[, col]) %in% c("factor", "character")) {
      factorrange[[colnames(genoutput)[col]]] = unique(genoutput[, col])
    }
    if (is.numeric(genoutput[, col])) {
      if (ncol(genoutput) == 1) {
        continuouslength = 51
      }
      factorrange[[colnames(genoutput)[col]]] = seq(-1, 1, length.out = continuouslength)
    }
  }
  fullgrid = expand.grid(factorrange)
  if (ncol(fullgrid) > 1) {
    samples = fullgrid[sample(1:nrow(fullgrid), 10000, replace = TRUE), ]
  } else {
    samples = data.frame(fullgrid[sample(1:nrow(fullgrid), 10000, replace = TRUE), ])
    colnames(samples) = colnames(fullgrid)
  }

  mm = model.matrix(model, genoutput, contrasts.arg = contrastlist)
  samplemm = model.matrix(model, samples, contrasts.arg = contrastlist)

  testcor = solve(t(mm) %*% solve(V) %*% mm)

  v = list()

  for (i in 1:nrow(samplemm)) {
    xi = samplemm[i, ]
    v[[i]] = t(xi) %*% testcor %*% xi
  }

  vars = do.call(rbind, v)
  varsordered = vars[order(vars)]
  meanindex = which(abs(mean(varsordered) - varsordered) == min(abs(mean(varsordered) - varsordered)))

  scale = varsordered[meanindex]
  if (length(scale) > 1) {
    scale = scale[1]
  }
  varsorderedscaled = varsordered / scale * Iopt
  midval = varsorderedscaled[5000]
  if(is.null(yaxis_max)) {
    maxyaxis = max(varsorderedscaled) + max(varsorderedscaled) / 20
  } else {
    maxyaxis = yaxis_max
  }
  if(plot) {
    plot(1:length(varsorderedscaled) / length(varsorderedscaled), varsorderedscaled, ylim = c(0, maxyaxis), type = "n",
         xlab = description, ylab = "Prediction Variance",
         xlim = c(0, 1), xaxs = "i", yaxs = "i")
    abline(v = 0.5, untf = FALSE, lty = 2, col = "red", lwd = 2)
    abline(h = midval, untf = FALSE, lty = 2, col = "red", lwd = 2)
    lines(1:length(varsorderedscaled) / length(varsorderedscaled), varsorderedscaled, lwd = 2, col = "blue")
    invisible(varsorderedscaled)
  } else {
    return(varsorderedscaled)
  }
}
