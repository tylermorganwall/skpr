#'@title Calculates Power Given a Run Matrix
#'
#'@description Evaluates a design given a run matrix and returns
#'a data frame of parameter and effect powers. Designs can
#'consist of both continuous and categorical factors. Default
#'assumes a signal-to-noise ratio of 2 (can be changed with the
#'delta parameter).
#'
#'@param RunMatrix The run matrix being evaluated.
#'@param model The model used in evaluating the design. It can be a subset of the model used to
#'generate the design, or include higher order effects not in the original design generation.
#'@param alpha The specified type-I error.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients will be
#'automatically generated.
#'@param delta The signal-to-noise ratio.Default 2. This specifies the difference between the high and low levels.
#'Anticipated coefficients will be half of this number.
#'@param contrasts A string specifying how to treat the contrasts in calculating the model matrix.
#'@param conservative Default FALSE. Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. TRUE will give the most conservative
#'estimate of power by setting all but one level in a categorical factor's anticipated coefficients
#'to zero.
#'@return A data frame with the parameters of the model, the type of power analysis, and the power.
#'@import AlgDesign
#'@export
#'@examples #Generating a simple 2x3 factorial using AlgDesign's gen.factorial function
#'#to feed into our optimal design generation and generating a 11-run design.
#'factorial <- AlgDesign::gen.factorial(levels = 2, nVars = 3, varNames = c("A", "B", "C"))
#'#this can also be generated with expand.grid as:
#'factorial <- expand.grid(A=c(1,-1),B=c(1,-1),C=c(1,-1))
#'optdesign = gen_design(factorial=factorial, model= ~A+B+C,trials=11,optimality="D",repeats=100)
#'
#'#Now evaluating that design (with default anticipated coefficients and a delta of 2):
#'eval_design(RunMatrix=optdesign, model= ~A+B+C, alpha=0.2)
#'
#'#Evaluating a subset of the design (changing the power due to a different number of
#'#degrees of freedom)
#'eval_design(RunMatrix=optdesign, model= ~A+C, alpha=0.2)
#'
#'#Halving the signal-to-noise ratio by setting a different delta (default is 2):
#'eval_design(RunMatrix=optdesign, model= ~A+B+C, alpha=0.2,delta=1)
#'
#'#With 3+ level categorical factors, the choice of anticipated coefficients directly changes the
#'#final power calculation. For the most conservative power calculation, that involves
#'#setting all anticipated coefficients in a factor to zero except for one. We can specify this
#'#option with the "conservative" argument.
#'
#'factorialcoffee = expand.grid(cost=c(1,2),
#'                              type=as.factor(c("Kona","Colombian","Ethiopian","Sumatra")),
#'                              size=as.factor(c("Short","Grande","Venti")))
#'
#'designcoffee = gen_design(factorialcoffee,~cost + size + type,trials=29,optimality="D",repeats=100)
#'
#'#Evaluate the design, with default anticipated coefficients (conservative is FALSE by default).
#'eval_design(designcoffee,model=~cost+size+type, alpha=0.05)
#'
#'#Evaluate the design, with conservative anticipated coefficients:
#'eval_design(designcoffee,model=~cost+size+type, alpha=0.05,conservative=TRUE)
#'
#'#which is the same as the following, but now explicitly entering the coefficients:
#'eval_design(designcoffee,model=~cost+size+type, alpha=0.05,anticoef=c(1,1,1,0,0,1,0))
#'
#'#If the first level in a factor is not the one that you want to set to one
#'#in the conservative calculation, enter the anticipated coefficients in manually.
#'eval_design(designcoffee,model=~cost+size+type, alpha=0.05,anticoef=c(1,1,0,0,1,0,1))
#'
#'#You can also evaluate the design with higher order effects:
#'eval_design(designcoffee,model=~cost+size+type+cost*type, alpha=0.05)

eval_design = function(RunMatrix, model, alpha, blockmodel=NULL, anticoef=NULL,
                       delta=2, varianceratio=1, contrasts="contr.sum", conservative=FALSE) {

  contrastslist = list()
  for(x in names(RunMatrix[sapply(RunMatrix,class) == "factor"])) {
    contrastslist[x] = contrasts
  }

  attr(RunMatrix,"modelmatrix") = model.matrix(model,RunMatrix,contrasts.arg=contrastslist)

  RunMatrix = reducemodelmatrix(RunMatrix,model)

  if(!is.null(blockmodel)) {
    BlockDesign = data.frame()
    block = floor(as.numeric(rownames(RunMatrix)))
    blocknumbers = unique(block)
    indices = c()
    for(blockstarts in blocknumbers) {
      indices = c(indices, match(blockstarts,block))
    }
    for(ind in indices) {
      BlockDesign = rbind(BlockDesign,RunMatrix[ind,])
    }
    names(BlockDesign) = names(RunMatrix)
    attr(BlockDesign,"modelmatrix") = model.matrix(model,BlockDesign,contrasts.arg=contrastslist)
  }


  if(!is.null(blockmodel)) {
    BlockedRunMatrix = reducemodelmatrix(BlockDesign, blockmodel)
    print(BlockedRunMatrix)
    if(any(lapply(BlockedRunMatrix,class) == "factor")) {
      blockedcontrastslist = list()
      for(x in names(BlockedRunMatrix[sapply(BlockedRunMatrix,class) == "factor"])) {
        blockedcontrastslist[x] = contrasts
      }
      attr(BlockedRunMatrix,"modelmatrix") = model.matrix(blockmodel,BlockedRunMatrix,contrasts.arg=blockedcontrastslist)
    } else {
      attr(BlockedRunMatrix,"modelmatrix") = model.matrix(blockmodel,BlockedRunMatrix)
    }
    blockedanticoef = gen_anticoef(BlockedRunMatrix,blockmodel,conservative=conservative)
  }

  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrix,model,conservative=conservative)
  }
  if(length(anticoef) != dim(attr(RunMatrix,"modelmatrix"))[2] && any(sapply(RunMatrix,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(attr(RunMatrix,"modelmatrix"))[2] && !any(sapply(RunMatrix,class)=="factor")) {
    anticoef = rep(1,dim(attr(RunMatrix,"modelmatrix"))[2])
  }

  if(!is.null(blockmodel)) {
    # varratio = c(rep(varianceratio,ncol(attr(BlockedRunMatrix,"modelmatrix"))),rep(0,length(anticoef)-ncol(attr(BlockedRunMatrix,"modelmatrix"))))
    varratio = c(rep(nrow(BlockedRunMatrix)/nrow(RunMatrix),ncol(attr(BlockedRunMatrix,"modelmatrix"))))
  }

  #This returns if everything is continuous (no categorical)
  if (!any(table(attr(attr(RunMatrix,"modelmatrix"),"assign")[-1])!=1)) {
    effectresults = parameterpower(RunMatrix,anticoef*delta/2,alpha)
    typevector = rep("effect.power",length(effectresults))
    namevector = colnames(attr(RunMatrix,"modelmatrix"))
    if(!is.null(blockmodel)) {
      blockeffectresults = parameterpower(BlockDesign,anticoef*delta/2,alpha,varianceratio=varratio)
      blocknamevector = colnames(attr(BlockDesign,"modelmatrix"))
      for(i in 1:length(blocknamevector)) {
        effectresults[namevector == blocknamevector[i]] = blockeffectresults[i]
      }
    }
    return(data.frame(parameters = namevector, type = typevector, power = effectresults))
  } else {
    levelvector = sapply(lapply(RunMatrix,unique),length)

    #generating number of categorical factors

    catornot = rep(0,length(sapply(RunMatrix,class)))
    catornot[sapply(RunMatrix,class) == "factor"] = 1
    priorcat = priorlevels(catornot)

    effectresults = effectpower(RunMatrix,levelvector,anticoef*delta/2,alpha,priorcat)
    parameterresults = parameterpower(RunMatrix,anticoef*delta/2,alpha)

    typevector = c(rep("effect.power",length(effectresults)),rep("parameter.power",length(parameterresults)))
    effectnamevector = c("(Intercept)",names(sapply(lapply(RunMatrix,unique),length)))
    parameternamevector = colnames(attr(RunMatrix,"modelmatrix"))
    namevector = c(effectnamevector,parameternamevector)
    powervector = c(effectresults,parameterresults)

    results = data.frame(parameters = namevector, type = typevector, power = powervector)

    if(!is.null(blockmodel)) {
      if(!any(table(attr(attr(BlockDesign,"modelmatrix"),"assign")[-1])!=1)) {
        blockeffectresults = parameterpower(BlockedRunMatrix,blockedanticoef*delta/2,alpha,varianceratio=varratio)
        blocknamevector = colnames(attr(BlockedRunMatrix, "modelmatrix"))
        cols = c()
        for(col in blocknamevector) {
          cols = c(cols, match(col,colnames(attr(BlockDesign, "modelmatrix"))))
        }
        for(i in cols) {
          results$power[results$parameters == blocknamevector[i] && results$type == "effect.power"] = blockeffectresults[i]
          results$power[results$parameters == blocknamevector[i] && results$type == "parameter.power"] = blockeffectresults[i]
        }
      } else {
        blockedlevelvector = sapply(lapply(BlockedRunMatrix,unique),length)
        blockedcatornot = rep(0,length(lapply(BlockedRunMatrix,class)))
        blockedcatornot[lapply(BlockedRunMatrix,class) == "factor"] = 1
        blockedpriorcat = priorlevels(blockedcatornot)
        blockeffectresults = effectpower(BlockedRunMatrix,blockedlevelvector,blockedanticoef*delta/2,alpha,blockedpriorcat)
        blockparameterresults = parameterpower(BlockedRunMatrix,blockedanticoef*delta/2,alpha,varianceratio=varratio)
        blocknamevector = colnames(attr(BlockedRunMatrix, "modelmatrix"))
        fullblocknamevector = colnames(attr(BlockedRunMatrix, "modelmatrix"))
        effects = colnames(BlockedRunMatrix)
        cols = c()

        for(i in 1:length(effects)) {
          results$power[results$parameters == effects[i]] = blockeffectresults[i]
        }
        #substitute parameter results

        for(col in blocknamevector) {
          cols = c(cols, match(col,colnames(attr(BlockedRunMatrix, "modelmatrix"))))
        }

        for(i in cols) {
          results$power[results$parameters == fullblocknamevector[i]] = blockparameterresults[i]
        }
      }
    }

    if(length(namevector) != length(typevector)) {
      warning("Number of names does not equal number of power calculations")
    }
    return(results)
  }
}
