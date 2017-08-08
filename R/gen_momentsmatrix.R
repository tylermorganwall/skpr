#'@title Generates the moment matrix
#'
#'@description Returns number of levels prior to each parameter
#'
#'@param Levelvector Returns a vector consisting of the number
#'@keywords internal
#'@return Returns a vector consisting of the number
#'of levels preceeding each parameter (including the intercept)
gen_momentsmatrix = function(modelfactors,levelvector,classvector) {
  #set numeric levels to one
  levelvector[!classvector] = 1

  #parse factor types
  isintercept = modelfactors == "(Intercept)"
  ishigherorder = lapply(strsplit(modelfactors,split="^",fixed=TRUE),length) > 1
  isinteraction = lapply(strsplit(modelfactors,split=":",fixed=TRUE),length) > 1
  isnumeric = modelfactors %in% names(classvector)[!classvector]
  islinear = rep(TRUE,length(modelfactors)) & !isintercept & !ishigherorder & !isinteraction
  isfullfactor = !isintercept & !ishigherorder & !isinteraction & !isnumeric

  linearfactors = modelfactors[isfullfactor]
  linearterms = modelfactors[islinear]

  if(!all(islinear) && all(isinteraction[-1])) {
    stop("No main effects in model. skpR only supports interactions between terms when their main effects are present.")
  }

  ordermatrix = matrix(0,nrow=length(linearterms),ncol = length(modelfactors))
  momentsmatrix = matrix(list(2),nrow=length(modelfactors),ncol=length(modelfactors))
  momentsmatrixresults = matrix(0,nrow=length(modelfactors),ncol=length(modelfactors))

  for(i in 1:length(which(islinear))) {
    ordermatrix[i,which(islinear)[i]] = 1
  }

  for(i in 1:length(linearterms)) {
    if(any(ishigherorder)) {
      for(j in 2:ncol(ordermatrix)) {
        if(length(strsplit(modelfactors[j], split=paste(linearterms[i],"^",sep=""),fixed=TRUE)[[1]]) > 1) {
          ordermatrix[i,j] = as.numeric(gsub("\\D","",strsplit(modelfactors[j],split="^",fixed=TRUE)[[1]][2]))
        }
      }
    }
  }
  for(i in 1:length(linearterms)) {
    for(j in 2:ncol(ordermatrix)) {
      if(isinteraction[j] && (linearterms[i] %in% strsplit(modelfactors[j],split=":",fixed=TRUE)[[1]])) {
        ordermatrix[i,j] = ordermatrix[i,j]+1
      }
    }
  }
  ordermatrix[,1] = 0

  for(i in 1:length(modelfactors)) {
    for(j in 1:length(modelfactors)) {
      momentsmatrix[i,j] = list(ordermatrix[,i]+ordermatrix[,j])

      if(any((ordermatrix[,i]+ordermatrix[,j]) %% 2 == 1)) {
        momentsmatrix[i,j] = 0
      } else {
        momentsmatrixresults[i,j] = prod(1/(momentsmatrix[i,j][[1]]+1))
      }
    }
  }

  for(i in 1:length(modelfactors)) {
    for(j in 1:length(modelfactors)) {
      if((isfullfactor[j] || isfullfactor[i]) && !(isinteraction[i] || isinteraction[j])) {
        momentsmatrixresults[i,j] = 3*momentsmatrixresults[i,j]
      } else {
        if((isfullfactor[j] || isfullfactor[i]) || (isinteraction[i] || isinteraction[j])) {
          term = strsplit(modelfactors[i],split=":",fixed=TRUE)[[1]]
          catterms = sum(term %in% linearfactors)
          momentsmatrixresults[i,j] = 3^catterms*momentsmatrixresults[i,j]
        }
      }
    }
  }
  return(momentsmatrixresults)
}
