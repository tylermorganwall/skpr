#'@title Generates optimal model matrix from factorial and model
#'
#'@description Creates design given a model and
#'desired number of runs, returning the model matrix. Currently
#'Used with eval_design/eval_design_mc to produce power estimations for designs.
#'Models with catagorical factors can only be linear and non-interacting.
#'
#'@param factorial A full factorial test matrix generated for the factors in the model.
#'@param model The model used to generate the test design.
#'@param trials The number of runs in the design.
#'@param optimality The optimality criterion (e.g. "D")
#'@param contrastslist A list of the contrasts (e.g. "contr.sum"). If none is provided, all are set
#'to "contr.sum").
#'@param repeats The number of times to repeat the search for the best optimal condition
#'@return The model matrix for the design, to be passed to eval_design. The model matrix
#'has various attributes (accessible with the attr function) that aid evaluation.
#'@import AlgDesign
#'@export
#'@examples #generate all combinations of the factors with expand.grid()
#'factorial = expand.grid(a=c(-1,0,1),b=c(1,2,3,4),c=c(-1,1),d=c(-1,1))
#'
#'#generate the design
#'designlinear = gen_design(factorial,~a+b+c+d,10,"D",100)
#'
#'#generate the design with a quadratic model
#'designquadratic = gen_design(factorial,~(a+b+c+d)^2,15,"D",100)
#'
#'mixedfactorial = expand.grid(a=as.factor(c(1,2,3,4,5,6)),b=c(1,0,-1),c=as.factor(c(1,2,3,4)),
#'                             d=c(-1,1), stringsAsFactors = TRUE)
#'mixeddesign = gen_design(mixedfactorial,~a+b+c+d,15,"D",100)
#'
#'#Evaluating the design can be done with eval_design and
#'#eval_design_mc/eval_design_parmc (Monte Carlo methods)


gen_design = function(factorial, model, trials, optimality, contrastslist,repeats) {
  if(missing(repeats)){
    repeats= 100
  }
  #replicates the pool of design points because AlgDesign samples without replacement
  factorial = do.call("rbind", replicate(trials/nrow(factorial)+1, factorial, simplify = FALSE))

  if(any(sapply(factorial,class) == "factor")) {
    if(missing(contrastslist)) {
      contrastslist = list()
      for(x in names(factorial[sapply(factorial,class) == "factor"])) {
        contrastslist[x] = "contr.sum"
      }
    }
    if(table(sapply(factorial,class) == "factor")["TRUE"] != length(contrastslist)) {
      #warning("Not all entries accounted for in contrast list, setting all to contr.sum")
      contrastslist = list()
      for(x in names(factorial[sapply(factorial,class) == "factor"])) {
        contrastslist[x] = "contr.sum"
      }
    }
    dfmodelmatrix = AlgDesign::optFederov(model,data=factorial,nTrials=trials,criterion = optimality,nRepeats = repeats)

    mm = model.matrix(model,dfmodelmatrix$design,contrasts.arg=contrastslist)
    attr(mm,"type") = sapply(factorial,class)
    attr(mm,"levels") = sapply(sapply(factorial,unique),length)
    attr(mm,"D") = dfmodelmatrix[["D"]]
    attr(mm,"A") = dfmodelmatrix[["A"]]
    attr(mm,"Ge") = dfmodelmatrix[["Ge"]]
    attr(mm,"Dea") = dfmodelmatrix[["Dea"]]
    attr(mm,"Design") = dfmodelmatrix[["design"]]

    return(mm)
  } else {

    dfmodelmatrix = AlgDesign::optFederov(model,data=factorial,nTrials=trials,criterion = optimality,nRepeats = repeats)

    mm = model.matrix(model,dfmodelmatrix$design)
    attr(mm,"type") = sapply(factorial,class)
    attr(mm,"levels") = sapply(sapply(factorial,unique),length)
    attr(mm,"D") = dfmodelmatrix[["D"]]
    attr(mm,"A") = dfmodelmatrix[["A"]]
    attr(mm,"Ge") = dfmodelmatrix[["Ge"]]
    attr(mm,"Dea") = dfmodelmatrix[["Dea"]]
    attr(mm,"Design") = dfmodelmatrix[["design"]]

    return(mm)
  }
}
