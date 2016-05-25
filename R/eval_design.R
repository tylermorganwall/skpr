#'@title Evaluates model matrix
#'
#'@description Evaluates design given a model matrix and returns
#'a data frame of parameter and effect powers. Currently only works with linear,
#'non-interacting models with catagorical variables, or
#'
#'@param RunMatrix The model matrix being evaluated
#'@param alpha The type-I error
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients will be generated with
#'gen_anticoef
#'@param snr Signal-to-noise ratio. Used with the anticipated coefficients in power calculation.
#'@param conservative Default FALSE. Specifies whether default anticipated coefficents should be conservative or not.
#'@return A data frame with the parameters of the model, the type of power analysis, and the power.
#'@export
#'@examples #generate the full factorial design for the factors in the model
#'factorial = expand.grid(a=c(-1,0,1),b=c(1,2,3,4),c=c(-1,1),d=c(-1,1))
#'
#'#generate the designs
#'designlinear = gen_design(factorial,~a+b+c+d,10,"D",100)
#'
#'#evaluate the design for a given alpha
#'eval_design(designlinear,alpha=0.05)
#'
#'#generate the full factorial design for the factors in the model (with catagorical)
#'factorialmixed = expand.grid(a=as.factor(c(1,2,3,4,5,6)),b=c(1,0,-1),c=as.factor(c(1,2,3,4)),
#'                             d=c(-1,1), stringsAsFactors = TRUE)
#'mixeddesign = gen_design(factorialmixed,~a+b+c+d,18,"D",100)
#'
#'#evaluate the design, with automatically computed anticipated coefficients
#'eval_design(mixeddesign,alpha=0.05,conservative=FALSE)
#'#which is the same as this, but now explicitly entering the coefficients
#'eval_design(mixeddesign,alpha=0.05,c(1,1,-1,1,-1,1,1,1,-1,1,1))
#'
#'#evaluate the design, but with custom coeffients set
#'#for the most conservative effect power calculation
#'#(setting each level in the catagorical factor to zero except for the first)
#'eval_design(mixeddesign,alpha=0.05,c(1,1,0,0,0,0,1,1,0,0,1))
#'#which can also be written as:
#'eval_design(mixeddesign,alpha=0.05,conservative=TRUE)
#'#autogenerating the conservative anticipated coefficients

eval_design = function(RunMatrix, model, alpha, anticoef, snr=1, conservative=FALSE) {
  if(is.null(attr(RunMatrix,"modelmatrix"))) {
    contrastslist = list()
    for(x in names(RunMatrix[sapply(RunMatrix,class) == "factor"])) {
      contrastslist[x] = "contr.sum"
    }
    if(length(contrastslist) == 0) {
      attr(RunMatrix,"modelmatrix") = model.matrix(model.matrix(model,RunMatrix))
    } else {
      attr(RunMatrix,"modelmatrix") = model.matrix(model.matrix(model,RunMatrix,contrasts.arg=contrastslist))
    }
  }

  #ModelMatrix = reducemodelmatrix(ModelMatrix,model)

  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrix,conservative=conservative)
  }
  if(length(anticoef) != dim(attr(RunMatrix,"modelmatrix"))[2] && any(sapply(RunMatrix,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(attr(RunMatrix,"modelmatrix"))[2] && !any(sapply(RunMatrix,class)=="factor")) {
    anticoef = rep(1,dim(attr(RunMatrix,"modelmatrix"))[2])
  }

  #This returns if everything is continuous (no catagorical)
  if (!any(table(attr(attr(RunMatrix,"modelmatrix"),"assign")[-1])!=1) ) {
    effectresults = parameterpower(RunMatrix,anticoef*snr,alpha)
    typevector = rep("effect.power",length(effectresults))
    namevector = colnames(attr(RunMatrix,"modelmatrix"))
    return(data.frame(parameters = namevector, type = typevector, power = effectresults))
  } else {
    levelvector = sapply(sapply(RunMatrix,unique),length)

    #generating number of catagorical factors

    catornot = rep(0,length(sapply(RunMatrix,class)))
    catornot[sapply(RunMatrix,class) == "factor"] = 1
    priorcat = priorlevels(catornot)

    effectresults = effectpower(RunMatrix,levelvector,anticoef*snr,alpha,priorcat)
    parameterresults = parameterpower(RunMatrix,anticoef*snr,alpha)

    typevector = c(rep("effect.power",length(effectresults)),rep("parameter.power",length(parameterresults)))
    powervector = c(effectresults,parameterresults)
    effectnamevector = c("(intercept)",names(sapply(sapply(RunMatrix,unique),length)))
    parameternamevector = colnames(attr(RunMatrix,"modelmatrix"))
    namevector = c(effectnamevector,parameternamevector)

    if(length(namevector) != length(typevector)) {
      warning("Number of names does not equal number of power calculations")
    }
    return(data.frame(parameters = namevector, type = typevector, power = powervector))
  }
}
