#'@title Plots design diagnostics
#'
#'@description Plots design diagnostics
#'
#'@param genoutput The run matrix
#'@return Plots design diagnostics
plot_fds = function(genoutput,model=NULL) {
  browser
  Iopt = attr(genoutput,"I")

  if(is.null(model)) {
    model = attr(genoutput,"generating.model")
  }
  if(!is.null(attr(genoutput,"runmatrix"))) {
    genoutput = attr(genoutput,"runmatrix")
  }

  factornames = colnames(genoutput)[apply(genoutput,2,class) %in% c("factor","character")]
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
    if(class(genoutput[,col]) == "numeric") {
      factorrange[[colnames(genoutput)[col]]] = seq(-1,1,0.01)
    }
  }
  fullgrid = expand.grid(factorrange)

  samples = fullgrid[sample(1:nrow(fullgrid),10000,replace=TRUE),]

  mm = model.matrix(model,genoutput,contrasts.arg = contrastlist)
  samplemm = model.matrix(model,samples,contrasts.arg = contrastlist)

  testcor = solve(t(mm) %*% mm)

  v = list()

  for(i in 1:nrow(samplesmm)) {
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
  lines(1:length(varsorderedscaled)/length(varsorderedscaled),varsorderedscaled,lwd = 2,col="blue")

  abline(v = 0.5, untf = FALSE,lty=2,col="red",lwd=2)
  abline(h = midval, untf = FALSE,lty=2,col="red",lwd=2)

}
