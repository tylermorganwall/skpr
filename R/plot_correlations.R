#'@title Plots design diagnostics
#'
#'@description Plots design diagnostics
#'
#'@param genoutput The run matrix
#'@return Plots design diagnostics
plot_correlations = function(genoutput,model=NULL) {

  cormat = attr(genoutput,"correlation.matrix")

  par(mar=c(5,3,7,0))
  image(t(cormat[ncol(cormat):1,]),x=1:ncol(cormat),y=1:ncol(cormat),zlim=c(0,1),asp=1,axes=F,
        col=colorRampPalette(colors=c("blue","white","red"))(101),xlab="",ylab="")
  axis(3,at=1:ncol(cormat),labels=colnames(cormat), pos=ncol(cormat)+1,las=2,hadj=0,cex.axis=0.8)
  axis(2,at=ncol(cormat):1, labels=colnames(cormat), pos=0,las=2,hadj=1,cex.axis=0.8)

  colMap <- colorRampPalette(c("blue","white","red" ))(101)
  legend(length(colnames(cormat))+0.2,length(colnames(cormat)),
         c("0","","","","","0.5","","","","","1.0"), title="|r|\n",
         fill = colMap[c(seq(1,101,10))], xpd=TRUE,bty="n",border=NA,y.intersp=0.3,x.intersp=0.1,cex=1)

}
