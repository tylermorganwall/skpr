#'@title Plots design diagnostics
#'
#'@description Plots design diagnostics
#'
#'@param genoutput The run matrix
#'@param model The model, by default uses the model used in eval_design or gen_design.
#'@param continuouslength Default 9. The precision of the continuous variables.
#'@return Plots design diagnostics
#'@import graphics grDevices
#'@export
#'@examples
#'#We can pass either the output of gen_design or eval_design to plot_correlations
#'#in order to obtain the correlation map. Passing the output of eval_design is useful
#'#if you want to plot the correlation map from an externally generated design.
#'
#'#First generate the design:
#'
#'candidatelist = expand.grid(X1=c(1,-1),X2 = c(1,-1))
#'
#'design = gen_design(candidatelist,~(X1+X2),15)
#'
#'plot_fds(design)
plot_fds = function(genoutput,model=NULL,continuouslength = 11) {

  #Remove skpr-generated REML blocking indicators if present
  if(!is.null(attr(genoutput,"splitanalyzable"))) {
    if(attr(genoutput,"splitanalyzable")) {
      allattr = attributes(genoutput)
      genoutput = genoutput[,-1:-length(allattr$splitcolumns)]
      allattr$names = allattr$names[-1:-length(allattr$splitcolumns)]
      attributes(genoutput) = allattr
    }
  }

  Iopt = attr(genoutput,"I")
  V = attr(genoutput,"variance.matrix")

  if(is.null(model)) {
    model = attr(genoutput,"generating.model")
  }
  if(!is.null(attr(genoutput,"runmatrix"))) {
    genoutput = attr(genoutput,"runmatrix")
  }

  factornames = colnames(genoutput)[unlist(lapply(genoutput,class)) %in% c("factor","character")]
  if(length(factornames) > 0) {
    contrastlist = list()
    for(name in 1:length(factornames)) {
      contrastlist[[factornames[name]]] = "contr.sum"
    }
  } else {
    contrastlist = NULL
  }
  factorrange = list()
  for(col in 1:ncol(genoutput)) {
    if(class(genoutput[,col]) %in% c("factor","character")) {
      factorrange[[colnames(genoutput)[col]]] = unique(genoutput[,col])
    }
    if(is.numeric(genoutput[,col])) {
      if(ncol(genoutput) == 1) {
        continuouslength = 51
      }
      factorrange[[colnames(genoutput)[col]]] = seq(-1,1,length.out=continuouslength)
    }
  }
  fullgrid = expand.grid(factorrange)
  if(ncol(fullgrid) > 1) {
    samples = fullgrid[sample(1:nrow(fullgrid),10000,replace=TRUE),]
  } else {
    samples = data.frame(fullgrid[sample(1:nrow(fullgrid),10000,replace=TRUE),])
    colnames(samples) = colnames(fullgrid)
  }

  mm = model.matrix(model,genoutput,contrasts.arg = contrastlist)
  samplemm = model.matrix(model,samples,contrasts.arg = contrastlist)

  testcor = solve(t(mm) %*% solve(V) %*% mm)

  v = list()

  for(i in 1:nrow(samplemm)) {
    xi = samplemm[i,]
    v[[i]] = t(xi) %*% testcor %*% xi
  }

  do.call(rbind,v) -> vars
  varsordered = vars[order(vars)]
  meanindex = which(abs(mean(varsordered)-varsordered)==min(abs(mean(varsordered)-varsordered)))

  scale = varsordered[meanindex]
  if(length(scale) > 1) {
    scale = scale[1]
  }
  varsorderedscaled = varsordered/scale*Iopt
  midval = varsorderedscaled[5000]
  maxyaxis = max(varsorderedscaled)+max(varsorderedscaled)/20

  plot(1:length(varsorderedscaled)/length(varsorderedscaled),varsorderedscaled,ylim=c(0,maxyaxis), type="n",
       xlab = "Fraction of Design Space", ylab = "Prediction Variance",
       xlim=c(0,1),xaxs = "i",yaxs = "i")
  abline(v = 0.5, untf = FALSE,lty=2,col="red",lwd=2)
  abline(h = midval, untf = FALSE,lty=2,col="red",lwd=2)
  lines(1:length(varsorderedscaled)/length(varsorderedscaled),varsorderedscaled,lwd = 2,col="blue")

}

