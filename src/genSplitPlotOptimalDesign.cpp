#include "optimalityfunctions.h"
#include "nullify_alg.h"
#include "design_utils.h"
#include <queue>
#include <vector>

using namespace Rcpp;

//`@title genSplitPlotOptimalDesign
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
                               const std::string condition, const Eigen::MatrixXd& momentsmatrix, Eigen::VectorXi& initialRows,
                               const Eigen::MatrixXd& blockedVar,
                               Eigen::MatrixXd aliasdesign, Eigen::MatrixXd aliascandidatelist, double minDopt, List interactions,
                               const Eigen::MatrixXd disallowed, const bool anydisallowed, double tolerance, int kexchange) {
  //Load the R RNG
  RNGScope rngScope;
  //check and log whether there are inter-strata interactions
  int numberinteractions = interactions.size();
  bool interstrata = (numberinteractions > 0);
  std::vector<Eigen::VectorXi> interactionColumns;
  if(interstrata) {
    interactionColumns.reserve(numberinteractions);
    for(int i = 0; i < numberinteractions; i++) {
      interactionColumns.push_back(as<Eigen::VectorXi>(interactions[i]));
    }
  }
  //Generate blocking structure inverse covariance matrix
  const Eigen::MatrixXd vInv = blockedVar.colPivHouseholderQr().inverse();
  Eigen::MatrixXd ones(initialdesign.rows(),1);
  ones.col(0).setOnes();
  //Checks if the initial matrix is singular. If so, randomly generates a new design nTrials times.
  for(int j = 1; j < candidatelist.cols(); j++) {
    if(ones.col(0).cwiseEqual(candidatelist.col(j)).all()) {
      Rcpp::stop("skpr: Singular model matrix from factor aliased into intercept, revise model");
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
  auto computeRowInteractions = [&](Eigen::MatrixXd& matrix, int rowIndex, int offset) {
    if(!interstrata) {
      return;
    }
    for(int idx = 0; idx < numberinteractions; ++idx) {
      const Eigen::VectorXi& cols = interactionColumns[idx];
      double value = matrix(rowIndex, cols(0) - 1);
      for(int kk = 1; kk < cols.size(); ++kk) {
        value *= matrix(rowIndex, cols(kk) - 1);
      }
      matrix(rowIndex, offset + idx) = value;
    }
  };
  //Calculate interaction terms of initial design.
  if(interstrata) {
    for(int i = 0; i < numberinteractions; i++) {
      const Eigen::VectorXi& cols = interactionColumns[i];
      interactiontemp = combinedDesign.col(cols(0) - 1);
      interactiontempalias = combinedAliasDesign.col(cols(0) - 1);
      for(int kk = 1; kk < cols.size(); kk++) {
        interactiontemp = interactiontemp.cwiseProduct(combinedDesign.col(cols(kk) - 1));
        interactiontempalias = interactiontempalias.cwiseProduct(combinedAliasDesign.col(cols(kk) - 1));
      }
      combinedDesign.col(blockedCols + designCols + i) = interactiontemp;
      combinedAliasDesign.col(blockedCols + designColsAlias + i) = interactiontempalias;
    }
  }
  Eigen::VectorXi candidateRow = initialRows;
  Eigen::VectorXi shuffledindices;
  if(nTrials < candidatelist.cols() + blockedCols + numberinteractions) {
    Rcpp::stop("skpr: Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
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
        for(int idx = 0; idx < numberinteractions; idx++) {
          const Eigen::VectorXi& cols = interactionColumns[idx];
          interactiontemp = combinedDesign.col(cols(0) - 1);
          interactiontempalias = combinedAliasDesign.col(cols(0) - 1);
          for(int k = 1; k < cols.size(); k++) {
            interactiontemp = interactiontemp.cwiseProduct(combinedDesign.col(cols(k) - 1));
            interactiontempalias = interactiontempalias.cwiseProduct(combinedAliasDesign.col(cols(k) - 1));
          }
          combinedDesign.col(blockedCols + designCols + idx) = interactiontemp;
          combinedAliasDesign.col(blockedCols + designColsAlias + idx) = interactiontempalias;
        }
      }
    }
  }
  // If still no non-singular design, returns NA.
  if (isSingularBlocked(combinedDesign,vInv)) {
    return(List::create(_["indices"] = NumericVector::get_na(), _["model_matrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
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
      std::priority_queue<std::pair<double, int>> q;
      float min_val = -INFINITY;
      int k = kexchange;
      if(kexchange != nTrials) {
        for (int i = 0; i < nTrials; i++) {
          float temp_val = -initialdesign.row(i) * vInv * initialdesign.row(i).transpose();
          if(temp_val == min_val) {
            k++;
          } else if(temp_val > min_val) {
            min_val = temp_val;
            k = kexchange;
          }
          q.push(std::pair<double, int>(temp_val, i));
        }
      } else {
        for (int i = 0; i < nTrials; i++) {
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
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          //Calculate interaction terms for sub-whole plot interactions
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              interactiontempval =  temp(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= temp(i,interactionColumns[k](kk)-1);
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
              interactiontempval =  combinedDesign(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= combinedDesign(i,interactionColumns[k](kk)-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
                interactiontempval =  temp(i,interactionColumns[k](0)-1);
                for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                  interactiontempval *= temp(i,interactionColumns[k](kk)-1);
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
              interactiontempval =  combinedDesign(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= combinedDesign(i,interactionColumns[k](kk)-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
        for (int j = 0; j < totalPoints; j++) {
          try {
            temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
            computeRowInteractions(temp, i, blockedCols + designCols);
            pointallowed = true;
            if(anydisallowed) {
              for(int k = 0; k < disallowed.rows(); k++) {
                if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                  pointallowed = false;
                }
              }
            }
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
          computeRowInteractions(combinedDesign, entryx, blockedCols + designCols);
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
              interactiontempval =  temp(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= temp(i,interactionColumns[k](kk)-1);
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
              interactiontempval =  combinedDesign(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= combinedDesign(i,interactionColumns[k](kk)-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
              interactiontempval =  temp(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= temp(i,interactionColumns[k](kk)-1);
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
              interactiontempval =  combinedDesign(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= combinedDesign(i,interactionColumns[k](kk)-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
              interactiontempval =  temp(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= temp(i,interactionColumns[k](kk)-1);
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
              interactiontempval =  combinedDesign(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= combinedDesign(i,interactionColumns[k](kk)-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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
              interactiontempval =  temp(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= temp(i,interactionColumns[k](kk)-1);
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
              interactiontempval =  combinedDesign(i,interactionColumns[k](0)-1);
              interactiontempvalalias = combinedAliasDesign(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= combinedDesign(i,interactionColumns[k](kk)-1);
                interactiontempvalalias *= combinedAliasDesign(i,interactionColumns[k](kk)-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
              combinedAliasDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
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

    Eigen::VectorXi candidateRowTemp = candidateRow;
    Eigen::VectorXi initialRowsTemp = initialRows;
    Eigen::MatrixXd combinedDesignTemp = combinedDesign;

    Eigen::VectorXi bestcandidaterow = candidateRowTemp;
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
                  interactiontempval =  temp(i,interactionColumns[k](0)-1);
                  interactiontempvalalias = tempalias(i,interactionColumns[k](0)-1);
                  for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                    interactiontempval *= temp(i,interactionColumns[k](kk)-1);
                    interactiontempvalalias *= tempalias(i,interactionColumns[k](kk)-1);
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
                interactiontempval =  combinedDesign(i,interactionColumns[k](0)-1);
                interactiontempvalalias = combinedAliasDesign(i,interactionColumns[k](0)-1);
                for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                  interactiontempval *= combinedDesign(i,interactionColumns[k](kk)-1);
                  interactiontempvalalias *= combinedAliasDesign(i,interactionColumns[k](kk)-1);
                }
                combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
                combinedAliasDesign(i,blockedCols+designCols + k) = interactiontempval;
              }
            }
            candidateRowTemp(i) = entryy+1;
            initialRowsTemp(i) = entryy+1;
          } else {
            candidateRowTemp(i) = initialRowsTemp(i);
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
              interactiontempval =  temp(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= temp(i,interactionColumns[k](kk)-1);
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
              interactiontempval =  combinedDesign(i,interactionColumns[k](0)-1);
              for(int kk = 1; kk < interactionColumns[k].size(); kk++) {
                interactiontempval *= combinedDesign(i,interactionColumns[k](kk)-1);
              }
              combinedDesign(i,blockedCols+designCols + k) = interactiontempval;
            }
          }
          candidateRow(i) = entryy+1;
          initialRows(i) = entryy+1;
        } else {
          candidateRow(i) = initialRows(i);
        }
      }
      newOptimum = calculateBlockedCustomOptimality(combinedDesign, customBlockedOpt, vInv);
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["model_matrix"] = combinedDesign, _["criterion"] = newOptimum));
}
