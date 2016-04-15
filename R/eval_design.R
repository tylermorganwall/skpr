#'@title Evaluates model matrix
#'
#'@description Evaluates design given a model matrix and returns
#'a data frame of parameter and effect powers.
#'
#'@param modelmatrix The model matrix being evaluated
#'@param alpha The type-I error
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients will be generated with
#'gen_anticoef
#'@return A data frame with the parameters, the type of power analysis, and the power
#'@export
#'@examples #generate the full factorial design for the factors in the model
#'factorial = expand.grid(a=c(-1,0,1),b=c(1,2,3,4),c=c(-1,1),d=c(-1,1))
#'
#'#generate the designs
#'designlinear = gen_design(dategc,~a+b+c+d,10,"D",100)
#'designquadratic = gen_design(dategc,~(a+b+c+d)^2,15,"D",100)
#'
#'#evaluate the design for a given alpha
#'eval_design(designlinear,alpha=0.05)
#'eval_design(designquadratic,alpha=0.05)
#'
#'#generate the full factorial design for the factors in the model (with catagorical)
#'factorialmixed = expand.grid(a=as.factor(c(1,2,3,4,5,6)),b=c(1,0,-1),c=as.factor(c(1,2,3,4)),d=c(-1,1), stringsAsFactors = TRUE)
#'mixeddesign = gen_design(factorialmixed,~a+b+c+d,18,"D",100)
#'
#'#evaluate the design, with automatically computed anticipated coefficients (using gen_anticoef)
#'eval_design(mixeddesign,alpha=0.05)
#'#which is the same as this, but we are now explicitly entering the coefficients
#'eval_design(mixeddesign,alpha=0.05,c(1,1,-1,1,-1,1,1,1,-1,1,1))
#'
#'#evaluate the design, but with custom coeffients set for the most conservative effect power calculation
#'#(setting each level in the catagorical factor to zero except for the first)
#'eval_design(mixeddesign,alpha=0.05,c(1,1,0,0,0,0,1,1,0,0,1))
#'#which can also be written as:
#'eval_design(mixeddesign,alpha=0.05,gen_conservative_anticoef(mixeddesign))

eval_design = function(modelmatrix, alpha,anticoef) {
  if(missing(anticoef)) {
    #warning("Anticipated coefficients missing, auto-generating")
    anticoef = gen_anticoef(modelmatrix)
  }
  if(length(anticoef) != dim(modelmatrix)[2] && any(attr(modelmatrix,"type")=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(modelmatrix)[2] && !any(attr(modelmatrix,"type")=="factor")) {
    anticoef = rep(1,dim(modelmatrix)[2])
  }

  #This returns if everything is continuous (no catagorical)
  if (!any(table(attr(modelmatrix,"assign")[-1])!=1) ) {
    effectresults = parameterpower(modelmatrix,anticoef,alpha)
    typevector = rep("effect.power",length(effectresults))
    namevector = colnames(modelmatrix)
    return(data.frame(parameters = namevector, type = typevector, power = effectresults))
  } else {
  #if (!any(table(attr(modelmatrix,"assign")[-1])==1) ) {

    levelvector = attr(modelmatrix,"levels")

    #generating number of catagorical factors

    catornot = rep(0,length(attr(modelmatrix,"type")))
    catornot[attr(modelmatrix,"type") == "factor"] = 1
    priorcat = priorlevels(catornot)

    effectresults = effectpower(modelmatrix,levelvector,anticoef,alpha,priorcat)
    parameterresults = parameterpower(modelmatrix,anticoef,alpha)

    typevector = c(rep("effect.power",length(effectresults)),rep("parameter.power",length(parameterresults)))
    powervector = c(effectresults,parameterresults)
    effectnamevector = c("(intercept)",names(attr(modelmatrix,"levels")))
    parameternamevector = colnames(modelmatrix)
    namevector = c(effectnamevector,parameternamevector)

    if(length(namevector) != length(typevector)) {
      warning("Number of names does not equal number of power calculations")
    }
    return(data.frame(parameters = namevector, type = typevector, power = powervector))
  }
}
