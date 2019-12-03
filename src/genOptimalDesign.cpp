#include "optimalityfunctions.h"
#include "nullify_alg.h"

using namespace Rcpp;

//`@title genOptimalDesign
//`@param initialdesign The initial randomly generated design.
//`@param candidatelist The full candidate set in model matrix form.
//`@param condition Optimality criterion.
//`@param momentsmatrix The moment matrix.
//`@param initialRows The rows from the candidate set chosen for initialdesign.
//`@param aliasdesign The initial design in model matrix form for the full aliasing model.
//`@param aliascandidatelist The full candidate set with the aliasing model in model matrix form.
//`@param minDopt Minimum D-optimality during an Alias-optimal search.
//`@param tolerance Stopping tolerance for fractional increase in optimality criteria.
//`@param augmentedrows The rows that are fixed during the design search.
//`@return List of design information.
// [[Rcpp::export]]
List genOptimalDesign(Eigen::MatrixXd initialdesign, const Eigen::MatrixXd& candidatelist,
                      const std::string condition,
                      const Eigen::MatrixXd& momentsmatrix, NumericVector initialRows,
                      Eigen::MatrixXd aliasdesign,
                      const Eigen::MatrixXd& aliascandidatelist,
                      double minDopt, double tolerance, int augmentedrows) {
  RNGScope rngScope;
  int nTrials = initialdesign.rows();
  double numberrows = initialdesign.rows();
  double numbercols = initialdesign.cols();
  int maxSingularityChecks = nTrials*100;
  int totalPoints = candidatelist.rows();
  Eigen::VectorXd candidateRow(nTrials);
  candidateRow.setZero();
  Eigen::MatrixXd test(initialdesign.cols(), initialdesign.cols());
  test.setZero();
  if(nTrials < candidatelist.cols()) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  //Check for singularity from a column perfectly correlating with the intercept.
  for(int j = 1; j < candidatelist.cols(); j++) {
    if(candidatelist.col(0).cwiseEqual(candidatelist.col(j)).all()) {
      throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
    }
  }
  Eigen::VectorXi shuffledindices;
  //Checks if the initial matrix is singular. If so, randomly generates a new design maxSingularityChecks times.
  for (int check = 0; check < maxSingularityChecks; check++) {
    if(!isSingular(initialdesign)) {
      break; //design is nonsingular
    }
    if(nTrials <= totalPoints) {
      shuffledindices = sample_noreplace(totalPoints, nTrials);
    } else {
      shuffledindices = sample_noreplace(nTrials, nTrials);
      for(int i = 0; i < shuffledindices.size(); i++) {
        shuffledindices(i) %= totalPoints;
      }
    }

    for (int i = augmentedrows; i < nTrials; i++) {
      initialdesign.row(i) = candidatelist.row(shuffledindices(i));
      aliasdesign.row(i) = aliascandidatelist.row(shuffledindices(i));
      initialRows(i) = shuffledindices(i) + 1; //R indexes start at 1
    }
  }
  //If initialdesign is still singular, use the Gram-Schmidt orthogonalization procedure, which
  //should return a non-singular matrix if one can be constructed from the candidate set
  if (isSingular(initialdesign)) {
    Eigen::VectorXi initrows = orthogonal_initial(candidatelist, nTrials);
    //If all elements are equal here, nullification algorithm was unable to find a design--return NA
    if(initrows.minCoeff() == initrows.maxCoeff()) {
      return(List::create(_["indices"] = NumericVector::get_na(), _["modelmatrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
    }

    //Replace non-augmented rows with orthogonal design
    for (int i = 0; i < nTrials - augmentedrows; i++) {
      initialdesign.row(i + augmentedrows) = candidatelist.row(initrows(i));
      aliasdesign.row(i + augmentedrows) = aliascandidatelist.row(initrows(i));
      initialRows(i + augmentedrows) = initrows(i) + 1; //R indexes start at 1
    }

    //Shuffle design
    Eigen::VectorXi initrows_shuffled = sample_noreplace(nTrials - augmentedrows, nTrials - augmentedrows);
    for (int i = augmentedrows; i < nTrials; i++) {
      initialdesign.row(i) = initialdesign.row(augmentedrows + initrows_shuffled(i));
      aliasdesign.row(i) = aliasdesign.row(augmentedrows + initrows_shuffled(i));
      initialRows(i) = augmentedrows + initrows_shuffled(i) + 1; //R indexes start at 1
    }
  }
  //If still no non-singular design, returns NA.
  if (isSingular(initialdesign)) {
    return(List::create(_["indices"] = NumericVector::get_na(), _["modelmatrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
  }

  bool found = true;
  double del = 0;
  int entryx = 0;
  int entryy = 0;
  double newOptimum = 0;
  double priorOptimum = 0;
  double minDelta = tolerance;
  double newdel;
  double xVx;

  //Initialize matrices for rank-2 updates.
  Eigen::MatrixXd identitymat(2,2);
  identitymat.setIdentity(2,2);
  Eigen::MatrixXd f1(initialdesign.cols(),2);
  Eigen::MatrixXd f2(initialdesign.cols(),2);
  Eigen::MatrixXd f2vinv(2,initialdesign.cols());

  //Transpose matrices for faster element access
  Eigen::MatrixXd initialdesign_trans = initialdesign.transpose();
  Eigen::MatrixXd candidatelist_trans = candidatelist.transpose();
  Eigen::MatrixXd V = (initialdesign.transpose()*initialdesign).partialPivLu().inverse();

  //Generate a D-optimal design
  if(condition == "D" || condition == "G") {
    newOptimum = calculateDOptimality(initialdesign);
    if(std::isinf(newOptimum)) {
      newOptimum = exp(calculateDOptimalityLog(initialdesign));
    }

    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();

        found = false;
        entryy = 0;
        del=0;
        xVx = initialdesign_trans.col(i).transpose() * V * initialdesign_trans.col(i);
        //Search through all candidate set points to find best switch (if one exists).

        search_candidate_set(V, candidatelist_trans, initialdesign_trans.col(i), xVx, entryy, found, del);

        if (found) {
          //Update the inverse with the rank-2 update formula.
          rankUpdate(V,initialdesign_trans.col(i),candidatelist_trans.col(entryy),identitymat,f1,f2,f2vinv);

          //Exchange points and re-calculate current criterion value.
          initialdesign_trans.col(i) = candidatelist_trans.col(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
          newOptimum = newOptimum * (1 + del);
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
    }
    initialdesign = initialdesign_trans.transpose();
    newOptimum = calculateDEff(initialdesign,numbercols,numberrows);
    if(std::isinf(newOptimum)) {
      newOptimum = calculateDEffLog(initialdesign,numbercols,numberrows);
    }
  }
  //Generate an I-optimal design
  if(condition == "I") {
    del = calculateIOptimality(V,momentsmatrix);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          newdel = calculateIOptimality(rankUpdateValue(V,initialdesign_trans.col(i),candidatelist_trans.col(j),identitymat,f1,f2,f2vinv),momentsmatrix);
          if(newdel < del) {
            found = true;
            entryx = i; entryy = j;
            del = newdel;
          }
        }
        if (found) {
          //Exchange points
          rankUpdate(V,initialdesign_trans.col(entryx),candidatelist_trans.col(entryy),identitymat,f1,f2,f2vinv);
          initialdesign_trans.col(entryx) = candidatelist_trans.col(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      //Re-calculate current criterion value
      initialdesign = initialdesign_trans.transpose();
      newOptimum = calculateIOptimality(V,momentsmatrix);
    }
  }
  //Generate an A-optimal design
  if(condition == "A") {
    del = calculateAOptimality(V);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          newdel = calculateAOptimality(rankUpdateValue(V,initialdesign_trans.col(i),candidatelist_trans.col(j),identitymat,f1,f2,f2vinv));
          if(newdel < del) {
            found = true;
            entryx = i; entryy = j;
            del = newdel;
          }
        }
        if (found) {
          //Exchange points
          rankUpdate(V,initialdesign_trans.col(entryx),candidatelist_trans.col(entryy),identitymat,f1,f2,f2vinv);
          initialdesign_trans.col(entryx) = candidatelist_trans.col(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      //Re-calculate current criterion value.
      initialdesign = initialdesign_trans.transpose();
      newOptimum = calculateAOptimality(V);
    }
  }
  //Generate an Alias optimal design
  if(condition == "ALIAS") {
    //First, calculate a D-optimal design (only do one iteration--may be only locally optimal) to start the search.
    Eigen::MatrixXd temp;
    Eigen::MatrixXd tempalias;
    del = calculateDOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;

    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryy = 0;
        del=0;
        xVx = initialdesign_trans.col(i).transpose() * V * initialdesign_trans.col(i);
        //Search through candidate set for potential exchanges for row i
        search_candidate_set(V, candidatelist_trans, initialdesign_trans.col(i), xVx, entryy, found, del);
        if (found) {
          //Update the inverse with the rank-2 update formula.
          rankUpdate(V,initialdesign_trans.col(i),candidatelist_trans.col(entryy),identitymat,f1,f2,f2vinv);

          //Exchange points and re-calculate current criterion value.
          initialdesign_trans.col(i) = candidatelist_trans.col(entryy);
          aliasdesign.row(i) = aliascandidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
          newOptimum = newOptimum * (1 + del);
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
    }
    initialdesign = initialdesign_trans.transpose();

    double firstA = calculateAliasTraceSlow(initialdesign,aliasdesign);
    double initialD = calculateDEffNN(initialdesign,numbercols);
    double currentA = firstA;
    double currentD = initialD;
    double wdelta = 0.05;
    double aliasweight = 1;
    double bestA = firstA;
    double optimum;
    double first = 1;

    Eigen::VectorXd candidateRowTemp = candidateRow;
    Eigen::Map<Eigen::VectorXd> initialRowsTemp = as<Eigen::Map<Eigen::VectorXd> >(initialRows);
    Eigen::MatrixXd initialdesignTemp = initialdesign;

    Eigen::VectorXd bestcandidaterow = candidateRowTemp;
    Eigen::MatrixXd bestaliasdesign = aliasdesign;
    Eigen::MatrixXd bestinitialdesign = initialdesign;

    //Perform weighted search, slowly increasing weight of Alias trace as compared to D-optimality.
    //Stops when the increase in the Alias-criterion is small enough. Does not make exchanges that
    //lower the D-optimal criterion below minDopt.
    while(firstA != 0 && currentA != 0 && aliasweight > wdelta) {

      //Set weight of Alias trace to D-optimality. Weight of Alias trace increases with each
      //iteration.
      aliasweight = aliasweight - wdelta;
      optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      first = 1;

      while((optimum - priorOptimum)/priorOptimum > minDelta || first == 1) {
        first++;
        priorOptimum = optimum;
        for (int i = augmentedrows; i < nTrials; i++) {
          Rcpp::checkUserInterrupt();
          found = false;
          entryx = 0;
          entryy = 0;
          temp = initialdesignTemp;
          tempalias = aliasdesign;
          //Search through candidate set for potential exchanges for row i
          for (int j = 0; j < totalPoints; j++) {
            temp.row(i) = candidatelist.row(j);
            tempalias.row(i) = aliascandidatelist.row(j);
            currentA = calculateAliasTrace(rankUpdateValue(V,initialdesign_trans.col(i),candidatelist_trans.col(j),identitymat,f1,f2,f2vinv),temp,tempalias);
            currentD = calculateDEffNN(temp,numbercols);
            newdel = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
            if(newdel > optimum && calculateDEff(temp,numbercols,numberrows) > minDopt) {
              found = true;
              entryx = i; entryy = j;
              optimum = newdel;
            }
          }
          if (found) {
            //Exchange points
            rankUpdate(V,initialdesign_trans.col(i),candidatelist_trans.col(entryy),identitymat,f1,f2,f2vinv);
            initialdesign_trans.col(entryx) = candidatelist_trans.col(entryy);
            initialdesignTemp.row(entryx) = candidatelist.row(entryy);
            aliasdesign.row(entryx) = aliascandidatelist.row(entryy);
            candidateRowTemp[i] = entryy+1;
            initialRowsTemp[i] = entryy+1;
          } else {
            candidateRowTemp[i] = initialRowsTemp[i];
          }
        }
        //Re-calculate current criterion value.
        currentD = calculateDEffNN(initialdesignTemp,numbercols);
        currentA = calculateAliasTraceSlow(initialdesignTemp,aliasdesign);
        optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      }
      //If the search improved the Alias trace, set that as the new value.
      if(currentA < bestA) {
        bestA = currentA;
        bestaliasdesign = aliasdesign;
        bestinitialdesign = initialdesignTemp;
        bestcandidaterow = candidateRowTemp;
      }
    }
    initialdesign = bestinitialdesign;
    candidateRow = bestcandidaterow;
    aliasdesign = bestaliasdesign;
    newOptimum = bestA;
  }
  if(condition == "G") {
    Eigen::MatrixXd temp;
    del = calculateGOptimality(V,initialdesign);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.row(i) = candidatelist.row(j);
            newdel = calculateGOptimality(rankUpdateValue(V,initialdesign_trans.col(i),candidatelist_trans.col(j),identitymat,f1,f2,f2vinv),temp);
            if(newdel < del) {
              if(!isSingular(temp)) {
                found = true;
                entryx = i; entryy = j;
                del = newdel;
              }
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          //Exchange points
          rankUpdate(V,initialdesign_trans.col(i),candidatelist_trans.col(entryy),identitymat,f1,f2,f2vinv);
          initialdesign.row(entryx) = candidatelist.row(entryy);
          initialdesign_trans.col(entryx) = candidatelist_trans.col(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      //Re-calculate current criterion value.
      newOptimum = calculateGOptimality(V,initialdesign);
    }
  }
  if(condition == "T") {
    Eigen::MatrixXd temp;
    del = calculateTOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.row(i) = candidatelist.row(j);
          newdel = calculateTOptimality(temp);
          if(newdel > del) {
            if(!isSingular(temp)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          //Exchange points
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      //Re-calculate current criterion value.
      newOptimum = calculateTOptimality(initialdesign);
    }
  }
  if(condition == "E") {
    Eigen::MatrixXd temp;
    del = calculateEOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.row(i) = candidatelist.row(j);
          newdel = calculateEOptimality(temp);
          if(newdel > del) {
            if(!isSingular(temp)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          //Exchange points
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      //Re-calculate current criterion value.
      newOptimum = calculateEOptimality(initialdesign);
    }
  }
  if(condition == "CUSTOM") {
    Environment myEnv = Environment::global_env();
    Function customOpt = myEnv["customOpt"];
    Eigen::MatrixXd temp;
    del = calculateCustomOptimality(initialdesign,customOpt);
    newOptimum = del;
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.row(i) = candidatelist.row(j);
          newdel = calculateCustomOptimality(temp,customOpt);
          if(newdel > del) {
            if(!isSingular(temp)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          //Exchange points
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      //Re-calculate current criterion value.
      newOptimum = calculateCustomOptimality(initialdesign,customOpt);
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = initialdesign, _["criterion"] = newOptimum));
}

//**********************************************************
//Everything below is for generating blocked optimal designs
//**********************************************************

//`@title genBlockedOptimalDesign
//`@param initialdesign The initial randomly generated design.
//`@param candidatelist The full candidate set in model matrix form.
//`@param blockeddesign The replicated and pre-set split plot design in model matrix form.
//`@param condition Optimality criterion.
//`@param momentsmatrix The moment matrix.
//`@param initialRows The rows from the candidate set chosen for initialdesign.
//`@param blockedVar Variance-covariance matrix calculated from the variance ratios between the split plot strata.
//`@param aliasdesign The initial design in model matrix form for the full aliasing model.
//`@param aliascandidatelist The full candidate set with the aliasing model in model matrix form.
//`@param minDopt Minimum D-optimality during an Alias-optimal search.
//`@param interactions List of integers pairs indicating columns of inter-strata interactions.
//`@param disallowed Matrix of disallowed combinations between whole-plots and sub-plots.
//`@param anydisallowed Boolean indicator for existance of disallowed combinations.
//`@param tolerance Stopping tolerance for fractional increase in optimality criteria.
//`@return List of design information.
// [[Rcpp::export]]
List genSplitPlotOptimalDesign(Eigen::MatrixXd initialdesign, Eigen::MatrixXd candidatelist, const Eigen::MatrixXd& blockeddesign,
                             const std::string condition, const Eigen::MatrixXd& momentsmatrix, IntegerVector initialRows,
                             const Eigen::MatrixXd& blockedVar,
                             Eigen::MatrixXd aliasdesign, Eigen::MatrixXd aliascandidatelist, double minDopt, List interactions,
                             const Eigen::MatrixXd disallowed, const bool anydisallowed, double tolerance) {
  //Load the R RNG
  RNGScope rngScope;
  //check and log whether there are inter-strata interactions
  int numberinteractions = interactions.size();
  bool interstrata = (numberinteractions > 0);
  Eigen::VectorXd orderinteraction(interactions.size());
  if(interstrata) {
    for(int i = 0; i < numberinteractions; i++) {
      orderinteraction[i] = as<NumericVector>(interactions[i]).size();
    }
  }

  //Generate blocking structure inverse covariance matrix
  const Eigen::MatrixXd vInv = blockedVar.colPivHouseholderQr().inverse();
  Eigen::MatrixXd ones(initialdesign.rows(),1);
  ones.col(0).setOnes();
  //Checks if the initial matrix is singular. If so, randomly generates a new design nTrials times.
  for(int j = 1; j < candidatelist.cols(); j++) {
    if(ones.col(0).cwiseEqual(candidatelist.col(j)).all()) {
      throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
    }
  }

  int nTrials = initialdesign.rows();
  int maxSingularityChecks = nTrials*10;
  int totalPoints = candidatelist.rows();
  int blockedCols = blockeddesign.cols();
  int designCols = initialdesign.cols();
  int designColsAlias = aliasdesign.cols();
  LogicalVector mustchange(nTrials, false);

  Eigen::MatrixXd combinedDesign(nTrials, blockedCols + designCols + numberinteractions);
  combinedDesign.setZero();
  combinedDesign.leftCols(blockedCols) = blockeddesign;
  combinedDesign.middleCols(blockedCols, designCols) = initialdesign;

  Eigen::MatrixXd combinedAliasDesign(nTrials, blockedCols + designColsAlias + numberinteractions);
  combinedAliasDesign.setZero();
  combinedAliasDesign.leftCols(blockedCols) = blockeddesign;
  combinedAliasDesign.middleCols(blockedCols, designColsAlias) = aliasdesign;

  Eigen::VectorXd interactiontemp(nTrials);
  Eigen::VectorXd interactiontempalias(nTrials);
  //Calculate interaction terms of initial design.
  if(interstrata) {
    for(int i = 0; i < numberinteractions; i++) {
      interactiontemp = combinedDesign.col(as<NumericVector>(interactions[i])[0]-1);
      interactiontempalias = combinedAliasDesign.col(as<NumericVector>(interactions[i])[0]-1);
      for(int kk = 1; kk < orderinteraction[i]; kk++) {
        interactiontemp = interactiontemp.cwiseProduct(combinedDesign.col(as<NumericVector>(interactions[i])[kk]-1));
        interactiontempalias = interactiontempalias.cwiseProduct(combinedAliasDesign.col(as<NumericVector>(interactions[i])[kk]-1));
      }
      combinedDesign.col(blockedCols+designCols + i) = interactiontemp;
      combinedAliasDesign.col(blockedCols+designColsAlias + i) = interactiontempalias;
    }
  }

  IntegerVector candidateRow = initialRows;
  Eigen::MatrixXd test(combinedDesign.cols(),combinedDesign.cols());
  test.setZero();
  Eigen::VectorXi shuffledindices;
  if(nTrials < candidatelist.cols() + blockedCols + numberinteractions) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  //Checks if the initial matrix is singular. If so, randomly generates a new design maxSingularityChecks times.
  for (int check = 0; check < maxSingularityChecks; check++) {
    if (!isSingularBlocked(combinedDesign,vInv)){
      break;
    }
    if(nTrials <= totalPoints) {
      shuffledindices = sample_noreplace(totalPoints, nTrials);
    } else {
      shuffledindices = sample_noreplace(nTrials, nTrials);
      for(int i = 0; i < shuffledindices.size(); i++) {
        shuffledindices(i) %= totalPoints;
      }
    }
    for (int i = 0; i < nTrials; i++) {
      candidateRow(i) = shuffledindices(i) + 1;
      initialRows(i) = shuffledindices(i) + 1;
      combinedDesign.block(i, blockedCols, 1, designCols) = candidatelist.row(shuffledindices(i));
      combinedAliasDesign.block(i, blockedCols, 1, designColsAlias) = aliascandidatelist.row(shuffledindices(i));
      if(interstrata) {
        for(int i = 0; i < numberinteractions; i++) {
          interactiontemp = combinedDesign.col(as<NumericVector>(interactions[i])[0]-1);
          interactiontempalias = combinedAliasDesign.col(as<NumericVector>(interactions[i])[0]-1);
          for(int k = 1; k < orderinteraction[i]; k++) {
            interactiontemp = interactiontemp.cwiseProduct(combinedDesign.col(as<NumericVector>(interactions[i])[k]-1));
            interactiontempalias = interactiontempalias.cwiseProduct(combinedAliasDesign.col(as<NumericVector>(interactions[i])[k]-1));
          }
          combinedDesign.col(blockedCols+designCols + i) = interactiontemp;
          combinedAliasDesign.col(blockedCols+designColsAlias + i) = interactiontempalias;
        }
      }
    }
  }

  // If still no non-singular design, returns NA.
  if (isSingularBlocked(combinedDesign,vInv)) {
    return(List::create(_["indices"] = NumericVector::get_na(), _["modelmatrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
  }
  // Checks for disallowed combinations, and marks those points as `mustchange` during the search if found
  if(anydisallowed) {
    for(int i = 0; i < nTrials; i++) {
      for(int j = 0; j < disallowed.rows(); j++) {
        if(combinedDesign.row(i).cwiseEqual(disallowed.row(j)).all()) {
          mustchange[i] = true;
        }
      }
    }
  }

  double del = 0;
  bool found = false;
  int entryx = 0;
  int entryy = 0;
  double newOptimum = 0;
  double priorOptimum = 0;
  double minDelta = tolerance;
  double newdel;
  double interactiontempval;
  double interactiontempvalalias;
  bool pointallowed = false;
  //Generate a D-optimal design, fixing the blocking factors
  if(condition == "D") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedDOptimality(combinedDesign, vInv);
    if(std::isinf(newOptimum)) {
      newOptimum = calculateBlockedDOptimalityLog(combinedDesign, vInv);
    }
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedDOptimality(combinedDesign,vInv);
      if(std::isinf(del)) {
        del = calculateBlockedDOptimalityLog(combinedDesign, vInv);
      }
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              temp(i,blockedCols+designCols + k) = interactiontempval;
            }
          }

          //Check for disallowed combinations
          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }

          //Check if optimality condition improved and can perform exchange
          newdel = calculateBlockedDOptimality(temp, vInv);
          if(std::isinf(newdel)) {
            newdel = calculateBlockedDOptimalityLog(temp, vInv);
          }
          if((newdel > del || mustchange[i]) && pointallowed) {
            found = true;
            entryx = i; entryy = j;
            del = newdel;
            mustchange[i] = false;
          }
        }
        if (found) {
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedDOptimality(combinedDesign, vInv);
      if(std::isinf(newOptimum)) {
        newOptimum = calculateBlockedDOptimalityLog(combinedDesign, vInv);
      }
    }
  }
  //Generate an I-optimal design, fixing the blocking factors
  if(condition == "I") {
    Eigen::MatrixXd temp;
    del = calculateBlockedIOptimality(combinedDesign, momentsmatrix, vInv);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
            //Calculate interaction terms for sub-whole plot interactions
            if(interstrata) {
              for(int k = 0; k < numberinteractions; k++) {
                interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
                for(int kk = 1; kk < orderinteraction[k]; kk++) {
                  interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
                }
                temp(i,blockedCols+designCols + k) = interactiontempval;
              }
            }
            //Check for disallowed combinations
            pointallowed = true;
            if(anydisallowed) {
              for(int k = 0; k < disallowed.rows(); k++) {
                if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                  pointallowed = false;
                }
              }
            }
            //Check if optimality condition improved and can perform exchange
            newdel = calculateBlockedIOptimality(temp,momentsmatrix,vInv);
            if((newdel < del || mustchange[i]) && pointallowed) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
              mustchange[i] = false;
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      try {
        newOptimum = calculateBlockedIOptimality(combinedDesign,momentsmatrix,vInv);
      } catch (std::runtime_error& e) {
        continue;
      }
    }
  }
  //Generate an A-optimal design, fixing the blocking factors
  if(condition == "A") {
    Eigen::MatrixXd temp;
    del = calculateBlockedAOptimality(combinedDesign,vInv);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
            //Calculate interaction terms for sub-whole plot interactions
            if(interstrata) {
              for(int k = 0; k < numberinteractions; k++) {
                interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
                for(int kk = 1; kk < orderinteraction[k]; kk++) {
                  interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
                }
                temp(i,blockedCols+designCols + k) = interactiontempval;
              }
            }
            //Check for disallowed combinations
            pointallowed = true;
            if(anydisallowed) {
              for(int k = 0; k < disallowed.rows(); k++) {
                if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                  pointallowed = false;
                }
              }
            }
            //Check if optimality condition improved and can perform exchange
            newdel = calculateBlockedAOptimality(temp,vInv);
            if((newdel < del || mustchange[i]) && pointallowed) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
              mustchange[i] = false;
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedAOptimality(combinedDesign,vInv);
    }
  }
  if(condition == "T") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedTOptimality(combinedDesign, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedTOptimality(combinedDesign,vInv);
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              temp(i,blockedCols+designCols + k) = interactiontempval;
            }
          }

          //Check for disallowed combinations
          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }
          //Check if optimality condition improved and can perform exchange
          newdel = calculateBlockedTOptimality(temp, vInv);
          if((newdel > del || mustchange[i]) && pointallowed) {
            if(!isSingularBlocked(temp,vInv)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
              mustchange[i] = false;
            }
          }
        }
        if (found) {
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedTOptimality(combinedDesign, vInv);
    }
  }

  //Generate an E-optimal design, fixing the blocking factors
  if(condition == "E") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedEOptimality(combinedDesign, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedEOptimality(combinedDesign,vInv);
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              temp(i,blockedCols+designCols + k) = interactiontempval;
            }
          }

          //Check for disallowed combinations
          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }
          //Check if optimality condition improved and can perform exchange
          try {
            newdel = calculateBlockedEOptimality(temp, vInv);
          } catch (std::runtime_error& e) {
            continue;
          }
          if((newdel > del || mustchange[i]) && pointallowed) {
            if(!isSingularBlocked(temp,vInv)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
              mustchange[i] = false;
            }
          }
        }
        if (found) {
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedEOptimality(combinedDesign, vInv);
    }
  }
  if(condition == "G") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedGOptimality(combinedDesign, vInv);
    priorOptimum = newOptimum*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      del = newOptimum;
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              temp(i,blockedCols+designCols + k) = interactiontempval;
            }
          }

          pointallowed = true;
          //Check for disallowed combinations
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }
          //Check if optimality condition improved and can perform exchange
          try {
            newdel = calculateBlockedGOptimality(temp, vInv);
          } catch (std::runtime_error& e) {
            continue;
          }
          if((newdel < del || mustchange[i]) && pointallowed) {
            if(!isSingularBlocked(temp,vInv)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
              mustchange[i] = false;
            }
          }
        }
        if (found) {
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedGOptimality(combinedDesign, vInv);
    }
  }

  if(condition == "ALIAS") {
    Eigen::MatrixXd temp;
    Eigen::MatrixXd tempalias;
    del = calculateBlockedDOptimality(combinedDesign,vInv);
    newOptimum = del;
    priorOptimum = newOptimum/2;

    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedDOptimality(combinedDesign,vInv);
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              temp(i,blockedCols+designCols + k) = interactiontempval;
            }
          }

          //Check for disallowed combinations
          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }
          //Check if optimality condition improved and can perform exchange
          newdel = calculateBlockedDOptimality(temp, vInv);
          if((newdel > del || mustchange[i]) && pointallowed) {
            found = true;
            entryx = i; entryy = j;
            del = newdel;
            mustchange[i] = false;
          }
        }
        if (found) {
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
          combinedAliasDesign.block(entryx, blockedCols, 1, designColsAlias) = aliascandidatelist.row(entryy);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
              interactiontempvalalias = combinedAliasDesign(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
                interactiontempvalalias *= combinedAliasDesign(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
              combinedAliasDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedDOptimality(combinedDesign, vInv);
    }

    double firstA = calculateBlockedAliasTracePseudoInv(combinedDesign,combinedAliasDesign,vInv);
    double initialD = calculateBlockedDEffNN(combinedDesign,vInv);
    double currentA = firstA;
    double currentD = initialD;
    double wdelta = 0.05;
    double aliasweight = 1;
    double bestA = firstA;
    double optimum;
    double first = 1;

    IntegerVector candidateRowTemp = candidateRow;
    IntegerVector initialRowsTemp = initialRows;
    Eigen::MatrixXd combinedDesignTemp = combinedDesign;

    IntegerVector bestcandidaterow = candidateRowTemp;
    Eigen::MatrixXd bestaliasdesign = combinedAliasDesign;
    Eigen::MatrixXd bestcombinedDesign = combinedDesign;

    while(firstA != 0 && currentA != 0 && aliasweight > wdelta) {

      aliasweight = aliasweight - wdelta;
      optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      first = 1;

      while((optimum - priorOptimum)/priorOptimum > minDelta || first == 1) {
        first++;
        priorOptimum = optimum;
        for (int i = 0; i < nTrials; i++) {
          Rcpp::checkUserInterrupt();
          found = false;
          entryx = 0;
          entryy = 0;
          temp = combinedDesignTemp;
          tempalias = combinedAliasDesign;
          //Search through candidate set for potential exchanges for row i
          for (int j = 0; j < totalPoints; j++) {
            try {
              temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
              tempalias.block(i, blockedCols, 1, designColsAlias) = aliascandidatelist.row(j);
              //Calculate interaction terms for sub-whole plot interactions
              if(interstrata) {
                for(int k = 0; k < numberinteractions; k++) {
                  interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
                  interactiontempvalalias = tempalias(i,as<NumericVector>(interactions[k])[0]-1);
                  for(int kk = 1; kk < orderinteraction[k]; kk++) {
                    interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
                    interactiontempvalalias *= tempalias(i,as<NumericVector>(interactions[k])[kk]-1);
                  }
                  temp(i,blockedCols+designCols + k) = interactiontempval;
                  tempalias(i,blockedCols+designColsAlias + k) = interactiontempvalalias;
                }
              }

              //Check for disallowed combinations
              pointallowed = true;
              if(anydisallowed) {
                for(int k = 0; k < disallowed.rows(); k++) {
                  if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                    pointallowed = false;
                  }
                }
              }
              //Check if optimality condition improved and can perform exchange
              currentA = calculateBlockedAliasTrace(temp,tempalias,vInv);
              currentD = calculateBlockedDEffNN(temp,vInv);
              newdel = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);

              if(newdel > optimum && calculateBlockedDEff(temp,vInv) > minDopt) {
                found = true;
                entryx = i; entryy = j;
                optimum = newdel;
              }
            } catch (std::runtime_error& e) {
              continue;
            }
          }
          if (found) {
            combinedDesignTemp.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
            combinedAliasDesign.block(entryx, blockedCols, 1, designColsAlias) = aliascandidatelist.row(entryy);
            //Calculate interaction terms for sub-whole plot interactions
            if(interstrata) {
              for(int k = 0; k < numberinteractions; k++) {
                interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
                interactiontempvalalias = combinedAliasDesign(i,as<NumericVector>(interactions[k])[0]-1);
                for(int kk = 1; kk < orderinteraction[k]; kk++) {
                  interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
                  interactiontempvalalias *= combinedAliasDesign(i,as<NumericVector>(interactions[k])[kk]-1);
                }
                combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
                combinedAliasDesign(i,blockedCols+designCols + k) = interactiontempval;
              }
            }
            candidateRowTemp[i] = entryy+1;
            initialRowsTemp[i] = entryy+1;
          } else {
            candidateRowTemp[i] = initialRowsTemp[i];
          }
        }
        currentD = calculateBlockedDEffNN(combinedDesignTemp,vInv);
        currentA = calculateBlockedAliasTrace(combinedDesignTemp,combinedAliasDesign,vInv);
        optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      }

      if(currentA < bestA) {
        bestA = currentA;
        bestaliasdesign = combinedAliasDesign;
        bestcombinedDesign = combinedDesignTemp;
        bestcandidaterow = candidateRowTemp;
      }
    }
    combinedDesign = bestcombinedDesign;
    candidateRow = bestcandidaterow;
    combinedAliasDesign = bestaliasdesign;
    newOptimum = bestA;
  }
  if(condition == "CUSTOM") {
    Environment myEnv = Environment::global_env();
    Function customBlockedOpt = myEnv["customBlockedOpt"];
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedCustomOptimality(combinedDesign, customBlockedOpt,vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedCustomOptimality(combinedDesign, customBlockedOpt, vInv);
      //Search through candidate set for potential exchanges for row i
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  temp(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= temp(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              temp(i,blockedCols+designCols + k) = interactiontempval;
            }
          }

          //Check for disallowed combinations
          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }
          //Check if optimality condition improved and can perform exchange
          try {
            newdel = calculateBlockedCustomOptimality(temp, customBlockedOpt, vInv);
          } catch (std::runtime_error& e) {
            continue;
          }
          if((newdel > del || mustchange[i]) && pointallowed) {
            if(!isSingularBlocked(temp,vInv)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
              mustchange[i] = false;
            }
          }
        }
        if (found) {
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidatelist.row(entryy);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  combinedDesign(i,as<NumericVector>(interactions[k])[0]-1);
              for(int kk = 1; kk < orderinteraction[k]; kk++) {
                interactiontempval *= combinedDesign(i,as<NumericVector>(interactions[k])[kk]-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedCustomOptimality(combinedDesign, customBlockedOpt, vInv);
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = combinedDesign, _["criterion"] = newOptimum));
}



//`@title genBlockedOptimalDesign
//`@param initialdesign The initial randomly generated design.
//`@param candidatelist The full candidate set in model matrix form.
//`@param condition Optimality criterion.
//`@param momentsmatrix The moment matrix.
//`@param initialRows The rows from the candidate set chosen for initialdesign.
//`@param aliasdesign The initial design in model matrix form for the full aliasing model.
//`@param aliascandidatelist The full candidate set with the aliasing model in model matrix form.
//`@param minDopt Minimum D-optimality during an Alias-optimal search.
//`@param tolerance Stopping tolerance for fractional increase in optimality criteria.
//`@param augmentedrows The rows that are fixed during the design search.
//`@return List of design information.
// [[Rcpp::export]]
List genBlockedOptimalDesign(Eigen::MatrixXd initialdesign, const Eigen::MatrixXd& candidatelist,
                      const std::string condition, Eigen::MatrixXd V,
                      const Eigen::MatrixXd& momentsmatrix, NumericVector initialRows,
                      Eigen::MatrixXd aliasdesign,
                      const Eigen::MatrixXd& aliascandidatelist,
                      double minDopt, double tolerance, int augmentedrows) {
  RNGScope rngScope;
  int nTrials = initialdesign.rows();
  double numbercols = initialdesign.cols();

  int maxSingularityChecks = nTrials*100;
  int totalPoints = candidatelist.rows();
  NumericVector candidateRow = initialRows;
  Eigen::MatrixXd test(initialdesign.cols(), initialdesign.cols());
  test.setZero();
  const Eigen::MatrixXd vInv = V.colPivHouseholderQr().inverse();

  if(nTrials < candidatelist.cols()) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  //Check for singularity from a column perfectly correlating with the intercept.
  for(int j = 1; j < candidatelist.cols(); j++) {
    if(candidatelist.col(0).cwiseEqual(candidatelist.col(j)).all()) {
      throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
    }
  }
  Eigen::VectorXi shuffledindices;
  //Checks if the initial matrix is singular. If so, randomly generates a new design maxSingularityChecks times.
  for (int check = 0; check < maxSingularityChecks; check++) {
    if(!isSingularBlocked(initialdesign,vInv)) {
      break; //design is nonsingular
    }
    if(nTrials <= totalPoints) {
      shuffledindices = sample_noreplace(totalPoints, nTrials);
    } else {
      shuffledindices = sample_noreplace(nTrials, nTrials);
      for(int i = 0; i < shuffledindices.size(); i++) {
        shuffledindices(i) %= totalPoints;
      }
    }

    for (int i = augmentedrows; i < nTrials; i++) {
      initialdesign.row(i) = candidatelist.row(shuffledindices(i));
      aliasdesign.row(i) = aliascandidatelist.row(shuffledindices(i));
      initialRows(i) = shuffledindices(i) + 1; //R indexes start at 1
    }
  }
  //If initialdesign is still singular, use the Gram-Schmidt orthogonalization procedure, which
  //should return a non-singular matrix if one can be constructed from the candidate set
  if (isSingularBlocked(initialdesign,vInv)) {
    Eigen::VectorXi initrows = orthogonal_initial(candidatelist, nTrials);
    //If all elements are equal here, nullification algorithm was unable to find a design--return NA
    if(initrows.minCoeff() == initrows.maxCoeff()) {
      return(List::create(_["indices"] = NumericVector::get_na(), _["modelmatrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
    }

    //Replace non-augmented rows with orthogonal design
    for (int i = 0; i < nTrials - augmentedrows; i++) {
      initialdesign.row(i + augmentedrows) = candidatelist.row(initrows(i));
      aliasdesign.row(i + augmentedrows) = aliascandidatelist.row(initrows(i));
      initialRows(i + augmentedrows) = initrows(i) + 1; //R indexes start at 1
    }

    //Shuffle design
    Eigen::VectorXi initrows_shuffled = sample_noreplace(nTrials - augmentedrows, nTrials - augmentedrows);
    for (int i = augmentedrows; i < nTrials; i++) {
      initialdesign.row(i) = initialdesign.row(augmentedrows + initrows_shuffled(i));
      aliasdesign.row(i) = aliasdesign.row(augmentedrows + initrows_shuffled(i));
      initialRows(i) = augmentedrows + initrows_shuffled(i) + 1; //R indexes start at 1
    }
  }
  //If still no non-singular design, returns NA.
  if (isSingularBlocked(initialdesign,vInv)) {
    return(List::create(_["indices"] = NumericVector::get_na(), _["modelmatrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
  }

  bool found = true;
  double del = 0;
  int entryx = 0;
  int entryy = 0;
  double newOptimum = 0;
  double priorOptimum = 0;
  double minDelta = tolerance;
  double newdel;

  //Initialize matrices for rank-2 updates.
  Eigen::MatrixXd identitymat(2,2);
  identitymat.setIdentity(2,2);
  Eigen::MatrixXd f1(initialdesign.cols(),2);
  Eigen::MatrixXd f2(initialdesign.cols(),2);
  Eigen::MatrixXd f2vinv(2,initialdesign.cols());

  //Transpose matrices for faster element access
  Eigen::MatrixXd initialdesign_trans = initialdesign.transpose();
  Eigen::MatrixXd candidatelist_trans = candidatelist.transpose();
  // Eigen::MatrixXd V = (initialdesign.transpose()*initialdesign).partialPivLu().inverse();

  //Generate a D-optimal design
  if(condition == "D") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedDOptimality(initialdesign, vInv);
    if(std::isinf(newOptimum)) {
      newOptimum = calculateBlockedDOptimalityLog(initialdesign, vInv);
    }
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedDOptimality(initialdesign,vInv);
      if(std::isinf(del)) {
        del = calculateBlockedDOptimalityLog(initialdesign, vInv);
      }
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
          //Check if optimality condition improved and can perform exchange
          newdel = calculateBlockedDOptimality(temp, vInv);
          if(std::isinf(newdel)) {
            newdel = calculateBlockedDOptimalityLog(temp, vInv);
          }
          if(newdel > del) {
            found = true;
            entryx = i; entryy = j;
            del = newdel;
          }
        }
        if (found) {
          initialdesign.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedDOptimality(initialdesign, vInv);
      if(std::isinf(newOptimum)) {
        newOptimum = calculateBlockedDOptimalityLog(initialdesign, vInv);
      }
    }
  }
  //Generate an I-optimal design
  if(condition == "I") {
    Eigen::MatrixXd temp;
    del = calculateBlockedIOptimality(initialdesign, momentsmatrix, vInv);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
            //Check if optimality condition improved and can perform exchange
            newdel = calculateBlockedIOptimality(temp,momentsmatrix,vInv);
            if(newdel < del) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          initialdesign.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      try {
        newOptimum = calculateBlockedIOptimality(initialdesign, momentsmatrix, vInv);
      } catch (std::runtime_error& e) {
        continue;
      }
    }
  }
  //Generate an A-optimal design
  if(condition == "A") {
    Eigen::MatrixXd temp;
    del = calculateBlockedAOptimality(initialdesign,vInv);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
            //Check if optimality condition improved and can perform exchange
            newdel = calculateBlockedAOptimality(temp,vInv);
            if(newdel < del) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          initialdesign.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedAOptimality(initialdesign,vInv);
    }
  }
  //Generate an Alias optimal design
  if(condition == "ALIAS") {
    Eigen::MatrixXd temp;
    Eigen::MatrixXd tempalias;
    del = calculateBlockedDOptimality(initialdesign,vInv);
    newOptimum = del;
    priorOptimum = newOptimum/2;

    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedDOptimality(initialdesign,vInv);
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
          //Calculate interaction terms for sub-whole plot interactions
          //Check if optimality condition improved and can perform exchange
          newdel = calculateBlockedDOptimality(temp, vInv);
          if(newdel > del) {
            found = true;
            entryx = i; entryy = j;
            del = newdel;
          }
        }
        if (found) {
          initialdesign.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
          aliasdesign.block(entryx, 0, 1, aliascandidatelist.cols()) = aliascandidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedDOptimality(initialdesign, vInv);
    }

    double firstA = calculateBlockedAliasTracePseudoInv(initialdesign, aliasdesign,vInv);
    double initialD = calculateBlockedDEffNN(initialdesign,vInv);
    double currentA = firstA;
    double currentD = initialD;
    double wdelta = 0.05;
    double aliasweight = 1;
    double bestA = firstA;
    double optimum;
    double first = 1;

    NumericVector candidateRowTemp = candidateRow;
    NumericVector initialRowsTemp = initialRows;
    Eigen::MatrixXd combinedDesignTemp = initialdesign;

    NumericVector bestcandidaterow = candidateRowTemp;
    Eigen::MatrixXd bestaliasdesign = aliasdesign;
    Eigen::MatrixXd bestcombinedDesign = initialdesign;

    while(firstA != 0 && currentA != 0 && aliasweight > wdelta) {

      aliasweight = aliasweight - wdelta;
      optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      first = 1;

      while((optimum - priorOptimum)/priorOptimum > minDelta || first == 1) {
        first++;
        priorOptimum = optimum;
        for (int i = 0; i < nTrials; i++) {
          Rcpp::checkUserInterrupt();
          found = false;
          entryx = 0;
          entryy = 0;
          temp = combinedDesignTemp;
          tempalias = aliasdesign;
          //Search through candidate set for potential exchanges for row i
          for (int j = 0; j < totalPoints; j++) {
            try {
              temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
              tempalias.block(i, 0, 1, aliascandidatelist.cols()) = aliascandidatelist.row(j);
              //Check if optimality condition improved and can perform exchange
              currentA = calculateBlockedAliasTrace(temp,tempalias,vInv);
              currentD = calculateBlockedDEffNN(temp,vInv);
              newdel = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);

              if(newdel > optimum && calculateBlockedDEff(temp,vInv) > minDopt) {
                found = true;
                entryx = i; entryy = j;
                optimum = newdel;
              }
            } catch (std::runtime_error& e) {
              continue;
            }
          }
          if (found) {
            combinedDesignTemp.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
            aliasdesign.block(entryx, 0, 1, aliascandidatelist.cols()) = aliascandidatelist.row(entryy);
            candidateRowTemp[i] = entryy+1;
            initialRowsTemp[i] = entryy+1;
          } else {
            candidateRowTemp[i] = initialRowsTemp[i];
          }
        }
        currentD = calculateBlockedDEffNN(combinedDesignTemp,vInv);
        currentA = calculateBlockedAliasTrace(combinedDesignTemp, aliasdesign,vInv);
        optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      }

      if(currentA < bestA) {
        bestA = currentA;
        bestaliasdesign = aliasdesign;
        bestcombinedDesign = combinedDesignTemp;
        bestcandidaterow = candidateRowTemp;
      }
    }
    initialdesign = bestcombinedDesign;
    candidateRow = bestcandidaterow;
    aliasdesign = bestaliasdesign;
    newOptimum = bestA;
  }
  if(condition == "G") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedGOptimality(initialdesign, vInv);
    priorOptimum = newOptimum*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      del = newOptimum;
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
          //Check if optimality condition improved and can perform exchange
          try {
            newdel = calculateBlockedGOptimality(temp, vInv);
          } catch (std::runtime_error& e) {
            continue;
          }
          if(newdel < del) {
            if(!isSingularBlocked(temp,vInv)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          initialdesign.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedGOptimality(initialdesign, vInv);
    }
  }

  if(condition == "T") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedTOptimality(initialdesign, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedTOptimality(initialdesign,vInv);
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
          //Check if optimality condition improved and can perform exchange
          newdel = calculateBlockedTOptimality(temp, vInv);
          if(newdel > del) {
            if(!isSingularBlocked(temp,vInv)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          initialdesign.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedTOptimality(initialdesign, vInv);
    }
  }
  if(condition == "E") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedEOptimality(initialdesign, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedEOptimality(initialdesign,vInv);
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
          //Check if optimality condition improved and can perform exchange
          try {
            newdel = calculateBlockedEOptimality(temp, vInv);
          } catch (std::runtime_error& e) {
            continue;
          }
          if(newdel > del) {
            if(!isSingularBlocked(temp,vInv)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          initialdesign.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedEOptimality(initialdesign, vInv);
    }
  }
  if(condition == "CUSTOM") {
    Environment myEnv = Environment::global_env();
    Function customBlockedOpt = myEnv["customBlockedOpt"];
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedCustomOptimality(initialdesign, customBlockedOpt,vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedCustomOptimality(initialdesign, customBlockedOpt, vInv);
      //Search through candidate set for potential exchanges for row i
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, 0, 1, numbercols) = candidatelist.row(j);
          //Check if optimality condition improved and can perform exchange
          try {
            newdel = calculateBlockedCustomOptimality(temp, customBlockedOpt, vInv);
          } catch (std::runtime_error& e) {
            continue;
          }
          if(newdel > del) {
            if(!isSingularBlocked(temp,vInv)) {
              found = true;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          initialdesign.block(entryx, 0, 1, numbercols) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedCustomOptimality(initialdesign, customBlockedOpt, vInv);
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = initialdesign, _["criterion"] = newOptimum));
}
