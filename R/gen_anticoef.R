#'@title Generates Anticipated Coefficients
#'
#'@description Generates Anticipated Coefficients
#'
#'@param RunMatrix The run matrix
#'@param conservative Default FALSE. If true, will generate conservative anticipated coefficients.
#'@return Anticipated coefficients.
#'@keywords internal
gen_anticoef = function(RunMatrix,model,conservative=FALSE) {
  #calculate levels for anticipated coefficients, with or without higher order effects
  if(max(attr(terms(model),"order")) == 1) {
    levels = sapply(lapply(RunMatrix,unique),length)-1
    type = sapply(RunMatrix,class)
  } else {
    levels = sapply(lapply(RunMatrix,unique),length)-1
    type = sapply(RunMatrix,class)
    notlinear = attr(terms(model),"order") > 1
    nonlinearterms = attr(terms(model),"term.labels")[notlinear]
    for(term in nonlinearterms) {
      higherlevel = 1
      highertype = "numeric"
      factors = strsplit(term,":")[[1]]
      for(i in factors) {
        higherlevel = (attr(RunMatrix,"out.attr")$dim[i]-1)*higherlevel
        if(class(RunMatrix[[i]]) == "factor") {
          highertype = "factor"
        }
      }
      levels = c(levels, higherlevel)
      type = c(type,highertype)
    }
  }

  anticoef = c(1)

  if(!conservative) {
    for(i in 1:length(levels)) {
      if (type[i] == "factor" && levels[i] %% 2 == 0) {
        anticoef = c(anticoef,rep(c(1,-1),levels[i]/2))
      }
      if (type[i] == "factor" && levels[i] %% 2 == 1) {
        anticoef = c(anticoef,1,rep(c(-1,1),(levels[i]-1)/2))
      }
      if (type[i] == "numeric" || type[i] == "integer") {
        anticoef = c(anticoef,1)
      }
    }
    return(anticoef)
  } else {
    for(i in 1:length(levels)) {
      if (type[i] == "factor") {
        anticoef = c(anticoef,1,rep(0,levels[i]-1))
      }
      if (type[i] == "numeric") {
        anticoef = c(anticoef,1)
      }
    }
    return(anticoef)
  }
}
