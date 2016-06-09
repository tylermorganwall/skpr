#'@title Generates the optimal run matrix from full factorial, model, optimality criterion,
#'and desired number of runs.
#'
#'@description Creates design given a model and desired number of runs, returning the model matrix. Currently
#'Used with eval_design/eval_design_mc to produce power estimations for designs.
#'
#'@param factorial A full factorial test matrix generated for the factors in the model. If
#'the factor is continuous, it should be type numeric. If the factor is categorical, it should be
#'set as a factor.
#'@param model The model used to generate the test design.
#'@param trials The number of runs in the design.
#'@param optimality The optimality criterion (e.g. "D")
#'@param contrastslist A list of the contrasts for categorical factors (e.g. "contr.sum").
#'If none are provided, all are set to "contr.sum".
#'@param repeats The number of times to repeat the search for the best optimal condition. If missing, this defaults to 100.
#'@param ... Any additional arguments to be input into AlgDesign's optFederov during design generation.
#'@return The model matrix for the design, to be passed to eval_design. The model matrix
#'has various attributes (accessible with the attr function) that aid evaluation.
#'@import AlgDesign
#'@export
#'@examples #Generate the basic factorial design used in generating the optimal design with
#'#expand.grid.
#'#Generating a basic 2 factor design:
#'basicdesign = expand.grid(x1=c(-1,1),x2=c(-1,1))
#'
#'#We can also generate the factorial using AlgDesign's gen.factorial function.
#'design_gf = AlgDesign::gen.factorial(levels=c(2,2),nVars=2,varNames=c("x1","x2"))
#'
#'#This factorial design is used as an input in the optimal design generation for a
#'#D-optimal design with 11 runs.
#'design = gen_design(factorial=basicdesign,model=~x1+x2,trials=11,optimality="D",repeats=100)
#'
#'#Here we add categorical factors, specified by using "as.factor" in expand.grid:
#'fulldesign = expand.grid(a=c(-1,1),b=as.factor(c("A","B")),c=as.factor(c("High","Med","Low")))
#'
#'#This factorial design is used as an input in the optimal design generation.
#'design2 = gen_design(factorial=fulldesign,model=~a+b+c,trials=19,optimality="D",repeats=100)
#'
#'#We can also decrease the number of repeats searches to increase speed (at the expense of
#'#possibly not having the best optimal design)
#'design2 = gen_design(factorial=fulldesign,model=~a+b+c,trials=19,optimality="D",repeats=5)
#'
#'#You can also use a higher order model when generating the design:
#'design2 = gen_design(factorial=fulldesign,model=~a+b+c+a*b*c,trials=19,optimality="D")
#'
#'#We can specify different methods of calculating contrasts for each factor in a list.
#'design2 = gen_design(factorial=fulldesign,model=~a+b+c,trials=19,optimality="D",
#'                    repeats=100,contrastslist=list(b="contr.sum",c="contr.treatment"))
#'
#'#The optimality numbers can be accessed as an attribute of the run matrix.
#'#See AlgDesign::optFederov documentation for more information.
#'attr(design2,"D") #The kth root of the generalized variance.
#'attr(design2,"A") #The average coefficient variance.
#'attr(design2,"Ge") #The minimax normalized variance.
#'attr(design2,"Dea") #A lower bound on D efficiency for approximate theory designs.
#'
#'#The I optimality number is not calculated unless the design is generated as I optimal:
#'design3 = gen_design(factorial=fulldesign,model=~a+b+c,trials=19,optimality="I",repeats=100)
#'attr(design3,"I") #The average prediction variance over X
#'
#'#Evaluating the design for power can be done with eval_design and eval_design_mc (Monte Carlo)


gen_design = function(factorial, model, trials, optimality="D", repeats=100,contrastslist, ...) {

  #replicates the pool of design points because AlgDesign samples without replacement
  factorial = do.call("rbind", replicate(trials, factorial, simplify = FALSE))

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
    dfmodelmatrix = AlgDesign::optFederov(model,data=factorial,nTrials=trials,
                                          criterion = optimality,nRepeats = repeats, ...)

    mm = dfmodelmatrix[["design"]]
    attr(mm,"D") = dfmodelmatrix[["D"]]
    attr(mm,"A") = dfmodelmatrix[["A"]]
    attr(mm,"I") = dfmodelmatrix[["I"]]
    attr(mm,"Ge") = dfmodelmatrix[["Ge"]]
    attr(mm,"Dea") = dfmodelmatrix[["Dea"]]

    return(mm)
  } else {

    dfmodelmatrix = AlgDesign::optFederov(model,data=factorial,nTrials=trials,
                                          criterion = optimality,nRepeats = repeats, ...)

    mm = dfmodelmatrix[["design"]]
    attr(mm,"D") = dfmodelmatrix[["D"]]
    attr(mm,"A") = dfmodelmatrix[["A"]]
    attr(mm,"I") = dfmodelmatrix[["I"]]
    attr(mm,"Ge") = dfmodelmatrix[["Ge"]]
    attr(mm,"Dea") = dfmodelmatrix[["Dea"]]

    return(mm)
  }
}
