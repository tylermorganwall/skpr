#include "optimalityfunctions.h"
#include "nullify_alg.h"

#include <RcppEigen.h>
#include <queue>

using namespace Rcpp;

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
                             const Eigen::MatrixXd& momentsmatrix,  Eigen::VectorXi& initialRows,
                             Eigen::MatrixXd aliasdesign,
                             const Eigen::MatrixXd& aliascandidatelist,
                             double minDopt, double tolerance, int augmentedrows, int kexchange) {
  RNGScope rngScope;
  int nTrials = initialdesign.rows();
  double numbercols = initialdesign.cols();

  int maxSingularityChecks = nTrials*100;
  int totalPoints = candidatelist.rows();
  Eigen::VectorXi candidateRow = initialRows;
  Eigen::MatrixXd test(initialdesign.cols(), initialdesign.cols());
  test.setZero();
  const Eigen::MatrixXd vInv = V.colPivHouseholderQr().inverse();

  if(nTrials < candidatelist.cols()) {
    Rcpp::stop("skpr: Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  //Check for singularity from a column perfectly correlating with the intercept.
  for(int j = 1; j < candidatelist.cols(); j++) {
    if(candidatelist.col(0).cwiseEqual(candidatelist.col(j)).all()) {
      Rcpp::stop("skpr: Singular model matrix from factor aliased into intercept, revise model");
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
      return(List::create(_["indices"] = NumericVector::get_na(), _["model_matrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
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
    return(List::create(_["indices"] = NumericVector::get_na(), _["model_matrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
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
      std::priority_queue<std::pair<double, int>> q;
      float min_val = -INFINITY;
      int k = kexchange - augmentedrows;
      if(kexchange != nTrials) {
        for (int i = augmentedrows; i < nTrials; i++) {
          float temp_val = -initialdesign_trans.col(i).transpose() * V * initialdesign_trans.col(i);
          if(temp_val == min_val) {
            k++;
          } else if(temp_val > min_val) {
            min_val = temp_val;
            k = kexchange - augmentedrows;
          }
          q.push(std::pair<double, int>(temp_val, i));
        }
      } else {
        for (int i = augmentedrows; i < nTrials; i++) {
          q.push(std::pair<double, int>(-i, i));
        }
      }

      for (int j = 0; j < k; j++) {
        Rcpp::checkUserInterrupt();
        int i = q.top().second;
        q.pop();
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
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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

    Eigen::VectorXi candidateRowTemp = candidateRow;
    Eigen::VectorXi initialRowsTemp = initialRows;
    Eigen::MatrixXd combinedDesignTemp = initialdesign;

    Eigen::VectorXi bestcandidaterow = candidateRowTemp;
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
            candidateRowTemp(i) = entryy+1;
            initialRowsTemp(i) = entryy+1;
          } else {
            candidateRowTemp(i) = initialRowsTemp(i);
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
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
        }
      }
      newOptimum = calculateBlockedCustomOptimality(initialdesign, customBlockedOpt, vInv);
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["model_matrix"] = initialdesign, _["criterion"] = newOptimum));
}
