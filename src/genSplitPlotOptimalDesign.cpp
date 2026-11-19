#include "optimalityfunctions.h"
#include "nullify_alg.h"
#include "design_utils.h"
#include <queue>
#include <vector>

using namespace Rcpp;

//`@title genSplitPlotOptimalDesign
//`@param initialdesign The initial randomly generated design.
//`@param candidatelist The full candidate set in model matrix form.
//`@param blockeddesign Model columns fixed at each run (whole plots or covariates).
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
//`@param covariateRows Optional run-specific candidate offsets and fixed alias columns for independent runs.
//`@return List of design information.
// [[Rcpp::export]]
List genSplitPlotOptimalDesign(Eigen::MatrixXd initialdesign, Eigen::MatrixXd candidatelist, const Eigen::MatrixXd& blockeddesign,
                               const std::string condition, const Eigen::MatrixXd& momentsmatrix, Eigen::VectorXi& initialRows,
                               const Eigen::MatrixXd& blockedVar,
                               Eigen::MatrixXd aliasdesign, Eigen::MatrixXd aliascandidatelist, double minDopt, List interactions,
                               const Eigen::MatrixXd disallowed, const bool anydisallowed, double tolerance, int kexchange,
                               Nullable<List> covariateRows = R_NilValue) {
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
  const int nTrials = initialdesign.rows();
  int totalPoints = candidatelist.rows();
  const int blockedCols = blockeddesign.cols();
  const int designCols = initialdesign.cols();
  const int designColsAlias = aliasdesign.cols();
  const int maxSingularityChecks = nTrials * 10;
  const bool runSpecific = covariateRows.isNotNull();
  Eigen::MatrixXd vInv = blockedVar;
  if (!blockedVar.isIdentity(0)) vInv = blockedVar.colPivHouseholderQr().inverse();
  Eigen::MatrixXd fixedAlias = blockeddesign;
  Eigen::VectorXi offsets = Eigen::VectorXi::Zero(nTrials);
  Eigen::VectorXi aliasOffsets = Eigen::VectorXi::Zero(nTrials);
  if (runSpecific) {
    const List rows(covariateRows);
    totalPoints = as<int>(rows["candidate_count"]);
    offsets = as<Eigen::VectorXi>(rows["offsets"]);
    aliasOffsets = as<Eigen::VectorXi>(rows["alias_offsets"]);
    fixedAlias = as<Eigen::MatrixXd>(rows["fixed_alias"]);
    if (totalPoints < 1 || offsets.size() != nTrials || aliasOffsets.size() != nTrials ||
        fixedAlias.rows() != nTrials || offsets.minCoeff() < 0 || aliasOffsets.minCoeff() < 0 ||
        offsets.maxCoeff() + totalPoints > candidatelist.rows() ||
        aliasOffsets.maxCoeff() + totalPoints > aliascandidatelist.rows()) {
      stop("skpr: Invalid covariate candidate dimensions.");
    }
  } else {
    for (int j = 1; j < candidatelist.cols(); ++j) {
      if ((candidatelist.col(j).array() == 1.0).all()) {
        stop("skpr: Singular model matrix from factor aliased into intercept, revise model");
      }
    }
  }
  const int fixedAliasCols = fixedAlias.cols();
  // Each offset selects the treatments evaluated at this run's fixed covariates.
  auto candidate = [&](int run, int point) { return candidatelist.row(offsets(run) + point); };
  auto aliasCandidate = [&](int run, int point) { return aliascandidatelist.row(aliasOffsets(run) + point); };
  LogicalVector mustchange(nTrials, false);

  Eigen::MatrixXd combinedDesign(nTrials, blockedCols + designCols + numberinteractions);
  combinedDesign.setZero();
  combinedDesign.leftCols(blockedCols) = blockeddesign;
  combinedDesign.middleCols(blockedCols, designCols) = initialdesign;

  Eigen::MatrixXd combinedAliasDesign(nTrials, fixedAliasCols + designColsAlias + numberinteractions);
  combinedAliasDesign.setZero();
  combinedAliasDesign.leftCols(fixedAliasCols) = fixedAlias;
  combinedAliasDesign.middleCols(fixedAliasCols, designColsAlias) = aliasdesign;
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
  if(nTrials < designCols + blockedCols + numberinteractions) {
    Rcpp::stop("skpr: Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  //Checks if the initial matrix is singular. If so, randomly generates a new design maxSingularityChecks times.
  for (int check = 0; check < maxSingularityChecks; check++) {
    if (!isSingularBlocked(combinedDesign,vInv)){
      break;
    }
    if (runSpecific) {
      shuffledindices.resize(nTrials);
      for (int i = 0; i < nTrials; ++i) shuffledindices(i) = R_unif_index(totalPoints);
    } else if(nTrials <= totalPoints) {
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
      combinedDesign.block(i, blockedCols, 1, designCols) = candidate(i, shuffledindices(i));
      combinedAliasDesign.block(i, fixedAliasCols, 1, designColsAlias) = aliasCandidate(i, shuffledindices(i));
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
  // Independent G searches start from a D design, as in genOptimalDesign.
  if(condition == "D" || (condition == "G" && runSpecific)) {
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
        const Eigen::MatrixXd information = combinedDesign.transpose() * vInv * combinedDesign;
        const Eigen::MatrixXd inverseInformation = information.llt().solve(
            Eigen::MatrixXd::Identity(information.rows(), information.cols()));
        for (int i = 0; i < nTrials; i++) {
          float temp_val = -combinedDesign.row(i) * inverseInformation * combinedDesign.row(i).transpose();
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
      for (int j = 0; j < k && !q.empty(); j++) {
        Rcpp::checkUserInterrupt();
        int i = q.top().second;
        q.pop();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        //Search through candidate set for potential exchanges for row i
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
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
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
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
            temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
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
            if((newdel < del || mustchange[i]) && pointallowed &&
               !isSingularBlocked(temp, vInv)) {
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
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
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
            temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
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
            if((newdel < del || mustchange[i]) && pointallowed &&
               !isSingularBlocked(temp, vInv)) {
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
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
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
          temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
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
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
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
          temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
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
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
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
    Eigen::MatrixXd predictionPoints;
    if (runSpecific) {
      predictionPoints = as<Eigen::MatrixXd>(as<List>(covariateRows)["prediction_points"]);
      if (predictionPoints.rows() == 0 || predictionPoints.cols() != combinedDesign.cols()) {
        stop("skpr: Invalid G prediction matrix for the covariate model.");
      }
    }
    auto scoreG = [&](const Eigen::MatrixXd& design) -> double {
      if (!runSpecific) return calculateBlockedGOptimality(design, vInv);
      const Eigen::MatrixXd information = design.transpose() * design;
      Eigen::LLT<Eigen::MatrixXd> chol(information);
      if (chol.info() != Eigen::Success) return R_PosInf;
      return calculateGOptimality(chol.solve(Eigen::MatrixXd::Identity(design.cols(), design.cols())),
                                  predictionPoints);
    };
    newOptimum = scoreG(combinedDesign);
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
          temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
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
            newdel = scoreG(temp);
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
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
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
      newOptimum = scoreG(combinedDesign);
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
          temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
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
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
          combinedAliasDesign.block(entryx, fixedAliasCols, 1, designColsAlias) = aliasCandidate(entryx, entryy);
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
              temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
              tempalias.block(i, fixedAliasCols, 1, designColsAlias) = aliasCandidate(i, j);
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

              if (newdel > optimum &&
                  currentD / nTrials > minDopt) {
                found = true;
                entryx = i; entryy = j;
                optimum = newdel;
              }
            } catch (std::runtime_error& e) {
              continue;
            }
          }
          if (found) {
            combinedDesignTemp.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
            combinedAliasDesign.block(entryx, fixedAliasCols, 1, designColsAlias) = aliasCandidate(entryx, entryy);
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
    Function customOpt = runSpecific ? as<Function>(as<List>(covariateRows)["custom"]) :
                                      as<Function>(myEnv["customBlockedOpt"]);
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedCustomOptimality(combinedDesign, customOpt, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedCustomOptimality(combinedDesign, customOpt, vInv);
      //Search through candidate set for potential exchanges for row i
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidate(i, j);
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
            newdel = calculateBlockedCustomOptimality(temp, customOpt, vInv);
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
          combinedDesign.block(entryx, blockedCols, 1, designCols) = candidate(entryx, entryy);
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
      newOptimum = calculateBlockedCustomOptimality(combinedDesign, customOpt, vInv);
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["model_matrix"] = combinedDesign, _["criterion"] = newOptimum));
}
