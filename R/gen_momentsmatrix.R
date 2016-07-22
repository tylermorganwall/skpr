#'@title Generates the moment matrix
#'
#'@description Returns number of levels prior to each parameter
#'
#'@param Levelvector Returns a vector consisting of the number
#'@keywords internal
#'@return Returns a vector consisting of the number
#'of levels preceeding each parameter (including the intercept)
gen_momentsmatrix = function(modelfactors) {
  isintercept = modelfactors == "(Intercept)"
  ishigherorder = lapply(strsplit(modelfactors,split="^",fixed=TRUE),length) > 1
  isinteraction = lapply(strsplit(modelfactors,split=":",fixed=TRUE),length) > 1
  islinear = rep(TRUE,length(modelfactors)) & !isintercept & !ishigherorder & !isinteraction
  linearterms = modelfactors[islinear]
  ordermatrix = matrix(0,nrow=length(linearterms),ncol = length(modelfactors))
  momentsmatrix = matrix(list(2),nrow=length(modelfactors),ncol=length(modelfactors))

  for(i in 1:length(linearterms)) {
    ordermatrix[i,strsplit(modelfactors, split=linearterms[i],fixed=TRUE) != modelfactors] = 1
  }
  for(i in 1:length(linearterms)) {
    if(any(lapply(strsplit(modelfactors, split=paste(linearterms[i],"^",sep=""),fixed=TRUE),length) > 1)) {
      for(j in 1:ncol(ordermatrix)) {
        if(length(strsplit(modelfactors[j], split=paste(linearterms[i],"^",sep=""),fixed=TRUE)[[1]]) > 1) {
          ordermatrix[i,j] = as.numeric(gsub("\\D","",strsplit(modelfactors[j],split="^",fixed=TRUE)[[1]][2]))
        }
      }
    }
  }
  print(ordermatrix)
  for(i in 1:length(modelfactors)) {
    for(j in 1:length(modelfactors)) {
      momentsmatrix[i,j] = list(ordermatrix[,i]+ordermatrix[,j])

      if(any((ordermatrix[,i]+ordermatrix[,j]) %% 2 == 1)) {
        momentsmatrix[i,j] = 0
      } else {
        momentsmatrix[i,j] = prod(1/(momentsmatrix[i,j][[1]]+1))
      }
    }
  }
  return(momentsmatrix)
}
