#'@title Plots design diagnostics
#'
#'@description Plots design diagnostics
#'
#'@param genoutput The run matrix
#'@param customcolors A vector of colors for customizing the appearance of the colormap
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
#'cardesign = gen_design(candidatelist,~(cost+type+color+year)^2,40)
#'plot_correlations(cardesign)
#'
#'#You can also pass a custom color map to be used with
#'plot_correlations(cardesign,customcolors=c("black","green","red"))
#'plot_correlations(cardesign,customcolors=c("red","orange","yellow","green","blue"))
plot_correlations = function(genoutput,customcolors=NULL) {

  if(is.null(customcolors)) {
    imagecolors = colorRampPalette(colors=c("blue","white","red"))(101)
  } else {
    imagecolors = colorRampPalette(customcolors)(101)
  }

  cormat = attr(genoutput,"correlation.matrix")

  par(mar=c(5,3,7,0))
  image(t(cormat[ncol(cormat):1,]),x=1:ncol(cormat),y=1:ncol(cormat),zlim=c(0,1),asp=1,axes=F,
        col=imagecolors,xlab="",ylab="")
  axis(3,at=1:ncol(cormat),labels=colnames(cormat), pos=ncol(cormat)+1,las=2,hadj=0,cex.axis=0.8)
  axis(2,at=ncol(cormat):1, labels=colnames(cormat), pos=0,las=2,hadj=1,cex.axis=0.8)

  legend(length(colnames(cormat))+0.2,length(colnames(cormat)),
         c("0","","","","","0.5","","","","","1.0"), title="|r|\n",
         fill = imagecolors[c(seq(1,101,10))], xpd=TRUE,bty="n",border=NA,y.intersp=0.3,x.intersp=0.1,cex=1)
  par(mar=c(5.1, 4.1, 4.1, 2.1))

}
