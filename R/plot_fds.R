#'@title Plots design diagnostics
#'
#'@description Plots design diagnostics
#'
#'@param genoutput The run matrix
#'@param model The model, by default uses the model used in eval_design or gen_design.
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
#'candidatelist = expand.grid(cost=c(15000,20000),year=c("2001","2002","2003","2004"),
#'                            type=c("SUV","Sedan","Hybrid"),color=c("red","black","white"))
#'
#'cardesign = gen_design(candidatelist,~(cost+type+color+year)^2,40)
#'
#'plot_fds(cardesign)
plot_fds = function(genoutput,model=NULL) {

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
    if(class(genoutput[,col]) == "numeric") {
      factorrange[[colnames(genoutput)[col]]] = seq(-1,1,0.01)
    }
  }
  fullgrid = expand.grid(factorrange)

  samples = fullgrid[sample(1:nrow(fullgrid),10000,replace=TRUE),]

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
