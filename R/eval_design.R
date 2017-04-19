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
#'@param blocking Default FALSE. If TRUE, eval_design will look at the rownames to determine blocking structure.
#'@param anticoef The anticipated coefficients for calculating the power. If missing, coefficients
#'will be automatically generated based on the delta argument.
#'@param delta The signal-to-noise ratio. Default 2. This specifies the difference between the high
#'and low levels. If you do not specify anticoef, the anticipated coefficients will be half of delta. If you do specify anticoef, leave delta at its default of 2.
#'@param varianceratios Default 1. The ratio of the whole plot variance to the run-to-run variance. For designs with more than one subplot
#'this ratio can be a vector specifying the variance ratio for each subplot. Otherwise, it will use a single value for all strata.
#'@param contrasts A string specifying how to treat the contrasts in calculating the model matrix.
#'@param conservative Default FALSE. Specifies whether default method for generating
#'anticipated coefficents should be conservative or not. TRUE will give the most conservative
#'estimate of power by setting all but one level in a categorical factor's anticipated coefficients
#'to zero.
#'@return A data frame with the parameters of the model, the type of power analysis, and the power.
#'@export
#'@examples #Generating a simple 2x3 factorial to feed into our optimal design generation
#'#of an 11-run design.
#'factorial <- expand.grid(A=c(1,-1),B=c(1,-1),C=c(1,-1))
#'
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
#'
#'#Split plot designs can also be evaluated by specifying the blocking model.
#'
#'#Generating split plot design
#'coffeeblocks = expand.grid(caffeine=c(1,-1))
#'coffeeblockdesign = gen_design(coffeeblocks, ~caffeine, trials=12)
#'coffeefinaldesign = gen_design(factorialcoffee, model=~cost+size+type,trials=36,
#'                               splitplotdesign=coffeeblockdesign, splitplotsizes=3)
#'
#'#Evaluating design
#'eval_design(coffeefinaldesign, ~cost+size+type + caffeine, 0.2, blocking = TRUE)
#'
#'#We can also evaluate the design with a custom ratio between the whole plot error to
#'#the run-to-run error.
#'eval_design(coffeefinaldesign, ~cost+size+type + caffeine, 0.2, blocking = TRUE,
#'            varianceratio=2)
#'
#'#If the design was generated outside of skpr and thus the row names do not have the
#'#blocking structure encoded already, the user can add these manually. For a 12-run
#'#design with 4 blocks, the rownames will be as follows:
#'
#'manualrownames = paste(c(1,1,1,2,2,2,3,3,3,4,4,4),rep(c(1,2,3),4),sep=".")
#'
#'#If we wanted to add this blocking structure to the design coffeeblockdesign, we would
#'#simply set the rownames to this character vector:
#'
#'rownames(coffeeblockdesign) = manualrownames
#'
#'#Deeper levels of blocking can be specified with additional periods.
eval_design = function(RunMatrix, model, alpha, blocking=FALSE, anticoef=NULL,
                       delta=2, varianceratios=1, contrasts=contr.sum, conservative=FALSE) {
  RunMatrix = reduceRunMatrix(RunMatrix,model)

  #---Develop contrast lists for model matrix---#
  contrastslist = list()
  contrastslist_correlationmatrix = list()
  for(x in names(RunMatrix[lapply(RunMatrix,class) == "factor"])) {
    contrastslist[[x]] = contrasts
    contrastslist_correlationmatrix[[x]] = contr.simplex
  }
  if(length(contrastslist) < 1) {
    contrastslist = NULL
    contrastslist_correlationmatrix = NULL
  }

  #------Normalize/Center numeric columns ------#
  for(column in 1:ncol(RunMatrix)) {
    if(class(RunMatrix[,column]) == "numeric") {
      RunMatrix[,column] = as.numeric(scale(RunMatrix[,column],scale=FALSE)/max(scale(RunMatrix[,column],scale=FALSE)))
    }
  }

  #-Generate Model Matrix & Anticipated Coefficients-#

  attr(RunMatrix,"modelmatrix") = model.matrix(model,RunMatrix,contrasts.arg=contrastslist)

  if(missing(anticoef)) {
    anticoef = gen_anticoef(RunMatrix,model,conservative=conservative)
  }

  if(length(anticoef) != dim(attr(RunMatrix,"modelmatrix"))[2] && any(sapply(RunMatrix,class)=="factor")) {
    stop("Wrong number of anticipated coefficients")
  }
  if(length(anticoef) != dim(attr(RunMatrix,"modelmatrix"))[2] && !any(sapply(RunMatrix,class)=="factor")) {
    anticoef = rep(1,dim(attr(RunMatrix,"modelmatrix"))[2])
  }

  #-----Generate V inverse matrix-----X

  if(blocking) {
    blocklist = strsplit(rownames(RunMatrix),".",fixed=TRUE)

    existingBlockStructure = do.call(rbind,blocklist)
    blockgroups = apply(existingBlockStructure,2,blockingstructure)

    blockMatrixSize = nrow(RunMatrix)
    V = diag(blockMatrixSize)
    blockcounter = 1
    if(length(blockgroups) == 1 | is.matrix(blockgroups)) {
      stop("No blocking detected. Specify block structure in row names or set blocking=FALSE")
    }
    if(length(blockgroups) > 2 && length(varianceratios) == 1) {
      warning("Only one variance ratio specified for several levels of blocking, applying that variance ratio to all substrata")
      varianceratios = rep(varianceratios,length(blockgroups))
    }
    if(length(blockgroups) > 2 && length(varianceratios) != 1 && length(blockgroups)-1 != length(varianceratios)) {
      stop("Wrong number of variance ratio specified. Either specify value for all blocking levels or one value for all blocks.")
    }
    blockgroups = blockgroups[-length(blockgroups)]
    for(block in blockgroups) {
      V[1:block[1],1:block[1]] =  V[1:block[1],1:block[1]]+varianceratios[blockcounter]
      placeholder = block[1]
      for(i in 2:length(block)) {
        V[(placeholder+1):(placeholder+block[i]),(placeholder+1):(placeholder+block[i])] = V[(placeholder+1):(placeholder+block[i]),(placeholder+1):(placeholder+block[i])] + varianceratios[blockcounter]
        placeholder = placeholder + block[i]
      }
      blockcounter = blockcounter+1
    }
    vInv = solve(V)
  } else {
    vInv = NULL
  }


  #This returns if everything is continuous (no categorical)
  if (!any(table(attr(attr(RunMatrix,"modelmatrix"),"assign")[-1])!=1)) {
    effectresults = parameterpower(RunMatrix,anticoef*delta/2,alpha,vInv = vInv)
    typevector = rep("effect.power",length(effectresults))
    namevector = colnames(attr(RunMatrix,"modelmatrix"))

    results = data.frame(parameters = namevector, type = typevector, power = effectresults)

    attr(results, "modelmatrix") = attr(RunMatrix,"modelmatrix")
    attr(results, "anticoef") = anticoef*delta/2

    modelmatrix_cor = model.matrix(model,RunMatrix,contrasts.arg=contrastslist_correlationmatrix)
    if(ncol(modelmatrix_cor) > 2) {
      correlation.matrix = abs(cov2cor(covarianceMatrix(modelmatrix_cor))[-1,-1])
      colnames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      rownames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
      attr(results,"correlation.matrix") = round(correlation.matrix,8)
    }

    attr(results,"generating.model") = model
    attr(results,"runmatrix") = RunMatrix

    mm = gen_momentsmatrix(colnames(attr(RunMatrix,"modelmatrix")),RunMatrix)

    attr(results,"moment.matrix") = mm
    attr(results,"A") = AOptimality(attr(RunMatrix,"modelmatrix"))
    if(!blocking) {
      attr(results,"I") = IOptimality(as.matrix(attr(RunMatrix,"modelmatrix")),momentsMatrix = mm, blockedVar=diag(nrow(attr(RunMatrix,"modelmatrix"))))
    } else {
      attr(results,"I") = IOptimality(as.matrix(attr(RunMatrix,"modelmatrix")),momentsMatrix = mm, blockedVar = V)
    }
    attr(results,"D") = 100*DOptimality(attr(RunMatrix,"modelmatrix"))^(1/ncol(attr(RunMatrix,"modelmatrix")))/nrow(attr(RunMatrix,"modelmatrix"))


    return(results)
  } else {
    levelvector = sapply(lapply(RunMatrix,unique),length)

    effectresults = effectpower(RunMatrix,levelvector,anticoef*delta/2,alpha,vInv=vInv)
    parameterresults = parameterpower(RunMatrix,anticoef*delta/2,alpha,vInv=vInv)

    typevector = c(rep("effect.power",length(effectresults)),rep("parameter.power",length(parameterresults)))
    effectnamevector = c("(Intercept)",names(sapply(lapply(RunMatrix,unique),length)))
    parameternamevector = colnames(attr(RunMatrix,"modelmatrix"))
    namevector = c(effectnamevector,parameternamevector)
    powervector = c(effectresults,parameterresults)

    results = data.frame(parameters = namevector, type = typevector, power = powervector)

    if(length(namevector) != length(typevector)) {
      warning("Number of names does not equal number of power calculations")
    }

    attr(results, "modelmatrix") = attr(RunMatrix,"modelmatrix")
    attr(results, "anticoef") = anticoef*delta/2

    modelmatrix_cor = model.matrix(model,RunMatrix,contrasts.arg=contrastslist_correlationmatrix)
    if(ncol(modelmatrix_cor) > 2) {
      tryCatch({
        correlation.matrix = abs(cov2cor(solve(t(modelmatrix_cor) %*% modelmatrix_cor))[-1,-1])
        colnames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
        rownames(correlation.matrix) = colnames(modelmatrix_cor)[-1]
        attr(results,"correlation.matrix") = round(correlation.matrix,8)
      }, error = function(e) {warning("Correlation matrix not calculated")})
    }
    attr(results,"generating.model") = model
    attr(results,"runmatrix") = RunMatrix

    mm = gen_momentsmatrix(colnames(attr(RunMatrix,"modelmatrix")),RunMatrix)

    attr(results,"moment.matrix") = mm
    attr(results,"A") = AOptimality(attr(RunMatrix,"modelmatrix"))
    if(!blocking) {
      attr(results,"I") = IOptimality(as.matrix(attr(RunMatrix,"modelmatrix")),momentsMatrix = mm, blockedVar=diag(nrow(attr(RunMatrix,"modelmatrix"))))
    } else {
      attr(results,"I") = IOptimality(as.matrix(attr(RunMatrix,"modelmatrix")),momentsMatrix = mm, blockedVar = V)
    }
    attr(results,"D") = 100*DOptimality(attr(RunMatrix,"modelmatrix"))^(1/ncol(attr(RunMatrix,"modelmatrix")))/nrow(attr(RunMatrix,"modelmatrix"))

    return(results)
  }
}
