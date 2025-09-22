#include "optimalityfunctions.h"
#include "nullify_alg.h"
#include "design_utils.h"

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

  double del = 0.0;
  double newOptimum = 0.0;
  double priorOptimum = 0.0;
  const double minDelta = tolerance;

  //Generate a D-optimal design
  if(condition == "D") {
    newOptimum = calculateBlockedDOptimality(initialdesign, vInv);
    if(std::isinf(newOptimum)) {
      newOptimum = calculateBlockedDOptimalityLog(initialdesign, vInv);
    }
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = calculateBlockedDOptimality(initialdesign, vInv);
      if(std::isinf(currentCriterion)) {
        currentCriterion = calculateBlockedDOptimalityLog(initialdesign, vInv);
      }

      std::priority_queue<std::pair<double, int>> q;
      double minVal = -INFINITY;
      int k = kexchange - augmentedrows;
      if(kexchange != nTrials) {
        for (int row = augmentedrows; row < nTrials; ++row) {
          const double tempVal = -initialdesign.row(row) * V * initialdesign.row(row).transpose();
          if(tempVal == minVal) {
            ++k;
          } else if(tempVal > minVal) {
            minVal = tempVal;
            k = kexchange - augmentedrows;
          }
          q.emplace(tempVal, row);
        }
      } else {
        for (int row = augmentedrows; row < nTrials; ++row) {
          q.emplace(-static_cast<double>(row), row);
        }
      }

      for (int exchange = 0; exchange < k && !q.empty(); ++exchange) {
        Rcpp::checkUserInterrupt();
        const int row = q.top().second;
        q.pop();

        double bestCriterion = currentCriterion;
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            initialdesign.row(row) = candidatelist.row(candidateIndex);
            candidateScore = calculateBlockedDOptimality(initialdesign, vInv);
            if(std::isinf(candidateScore)) {
              candidateScore = calculateBlockedDOptimalityLog(initialdesign, vInv);
            }
            return true;
          },
          skpr::OptimizationSense::Maximize,
          currentCriterion,
          bestCriterion
        );

        initialdesign.row(row) = originalRow;

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow(row) = bestCandidate + 1;
          initialRows(row) = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow(row) = initialRows(row);
        }
      }

      newOptimum = calculateBlockedDOptimality(initialdesign, vInv);
      if(std::isinf(newOptimum)) {
        newOptimum = calculateBlockedDOptimalityLog(initialdesign, vInv);
      }
    }
  }
  //Generate an I-optimal design
  else if(condition == "I") {
    del = calculateBlockedIOptimality(initialdesign, momentsmatrix, vInv);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = del;
      for (int row = 0; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            try {
              initialdesign.row(row) = candidatelist.row(candidateIndex);
              candidateScore = calculateBlockedIOptimality(initialdesign, momentsmatrix, vInv);
            } catch (std::runtime_error&) {
              return false;
            }
            return true;
          },
          skpr::OptimizationSense::Minimize,
          currentCriterion,
          bestCriterion
        );

        initialdesign.row(row) = originalRow;

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow(row) = bestCandidate + 1;
          initialRows(row) = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow(row) = initialRows(row);
        }
      }

      try {
        newOptimum = calculateBlockedIOptimality(initialdesign, momentsmatrix, vInv);
      } catch (std::runtime_error&) {
        continue;
      }
      del = newOptimum;
    }
  }
  //Generate an A-optimal design
  else if(condition == "A") {
    del = calculateBlockedAOptimality(initialdesign, vInv);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = del;
      for (int row = 0; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            try {
              initialdesign.row(row) = candidatelist.row(candidateIndex);
              candidateScore = calculateBlockedAOptimality(initialdesign, vInv);
            } catch (std::runtime_error&) {
              return false;
            }
            return true;
          },
          skpr::OptimizationSense::Minimize,
          currentCriterion,
          bestCriterion
        );

        initialdesign.row(row) = originalRow;

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow(row) = bestCandidate + 1;
          initialRows(row) = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow(row) = initialRows(row);
        }
      }
      newOptimum = calculateBlockedAOptimality(initialdesign, vInv);
      del = newOptimum;
    }
  }
  //Generate an Alias optimal design
  else if(condition == "ALIAS") {
    newOptimum = calculateBlockedDOptimality(initialdesign, vInv);
    if(std::isinf(newOptimum)) {
      newOptimum = calculateBlockedDOptimalityLog(initialdesign, vInv);
    }
    priorOptimum = newOptimum/2;

    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = calculateBlockedDOptimality(initialdesign, vInv);
      if(std::isinf(currentCriterion)) {
        currentCriterion = calculateBlockedDOptimalityLog(initialdesign, vInv);
      }

      for (int row = 0; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            initialdesign.row(row) = candidatelist.row(candidateIndex);
            candidateScore = calculateBlockedDOptimality(initialdesign, vInv);
            if(std::isinf(candidateScore)) {
              candidateScore = calculateBlockedDOptimalityLog(initialdesign, vInv);
            }
            return true;
          },
          skpr::OptimizationSense::Maximize,
          currentCriterion,
          bestCriterion
        );

        initialdesign.row(row) = originalRow;

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          aliasdesign.row(row) = aliascandidatelist.row(bestCandidate);
          candidateRow(row) = bestCandidate + 1;
          initialRows(row) = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow(row) = initialRows(row);
        }
      }

      newOptimum = calculateBlockedDOptimality(initialdesign, vInv);
      if(std::isinf(newOptimum)) {
        newOptimum = calculateBlockedDOptimalityLog(initialdesign, vInv);
      }
    }

    const double firstA = calculateBlockedAliasTracePseudoInv(initialdesign, aliasdesign, vInv);
    const double initialD = calculateBlockedDEffNN(initialdesign, vInv);
    double currentA = firstA;
    double currentD = initialD;
    const double wdelta = 0.05;
    double aliasweight = 1.0;
    double bestA = firstA;
    double optimum = 0.0;
    double first = 1.0;

    Eigen::VectorXi candidateRowTemp = candidateRow;
    Eigen::VectorXi initialRowsTemp = initialRows;
    Eigen::MatrixXd combinedDesignTemp = initialdesign;

    Eigen::VectorXi bestcandidaterow = candidateRowTemp;
    Eigen::MatrixXd bestaliasdesign = aliasdesign;
    Eigen::MatrixXd bestcombinedDesign = initialdesign;

    while(firstA != 0 && currentA != 0 && aliasweight > wdelta) {

      aliasweight -= wdelta;
      optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      first = 1.0;

      while((optimum - priorOptimum)/priorOptimum > minDelta || first == 1.0) {
        ++first;
        priorOptimum = optimum;
        for (int row = 0; row < nTrials; ++row) {
          Rcpp::checkUserInterrupt();
          const Eigen::RowVectorXd originalDesignRow = combinedDesignTemp.row(row);
          const Eigen::RowVectorXd originalAliasRow = aliasdesign.row(row);
          double bestOptimum = optimum;

          const int bestCandidate = skpr::findBestCandidate(
            totalPoints,
            [&](int candidateIndex, double& candidateScore) -> bool {
              try {
                combinedDesignTemp.row(row) = candidatelist.row(candidateIndex);
                aliasdesign.row(row) = aliascandidatelist.row(candidateIndex);
                double candidateA = calculateBlockedAliasTrace(combinedDesignTemp, aliasdesign, vInv);
                double candidateD = calculateBlockedDEffNN(combinedDesignTemp, vInv);
                if(calculateBlockedDEff(combinedDesignTemp, vInv) <= minDopt) {
                  combinedDesignTemp.row(row) = originalDesignRow;
                  aliasdesign.row(row) = originalAliasRow;
                  return false;
                }
                candidateScore = aliasweight*candidateD/initialD + (1-aliasweight)*(1-candidateA/firstA);
              } catch (std::runtime_error&) {
                combinedDesignTemp.row(row) = originalDesignRow;
                aliasdesign.row(row) = originalAliasRow;
                return false;
              }
              combinedDesignTemp.row(row) = originalDesignRow;
              aliasdesign.row(row) = originalAliasRow;
              return true;
            },
            skpr::OptimizationSense::Maximize,
            optimum,
            bestOptimum
          );

          if (bestCandidate != -1) {
            combinedDesignTemp.row(row) = candidatelist.row(bestCandidate);
            aliasdesign.row(row) = aliascandidatelist.row(bestCandidate);
            candidateRowTemp(row) = bestCandidate + 1;
            initialRowsTemp(row) = bestCandidate + 1;
            optimum = bestOptimum;
          } else {
            combinedDesignTemp.row(row) = originalDesignRow;
            aliasdesign.row(row) = originalAliasRow;
            candidateRowTemp(row) = initialRowsTemp(row);
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
  else if(condition == "G") {
    newOptimum = calculateBlockedGOptimality(initialdesign, vInv);
    priorOptimum = newOptimum*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = newOptimum;
      for (int row = 0; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            bool success = false;
            try {
              initialdesign.row(row) = candidatelist.row(candidateIndex);
              if(!isSingularBlocked(initialdesign, vInv)) {
                candidateScore = calculateBlockedGOptimality(initialdesign, vInv);
                success = true;
              }
            } catch (std::runtime_error&) {
              success = false;
            }
            initialdesign.row(row) = originalRow;
            return success;
          },
          skpr::OptimizationSense::Minimize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow(row) = bestCandidate + 1;
          initialRows(row) = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow(row) = initialRows(row);
        }
      }
      newOptimum = calculateBlockedGOptimality(initialdesign, vInv);
    }
  }

  else if(condition == "T") {
    newOptimum = calculateBlockedTOptimality(initialdesign, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = calculateBlockedTOptimality(initialdesign, vInv);
      for (int row = 0; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            bool success = false;
            try {
              initialdesign.row(row) = candidatelist.row(candidateIndex);
              if(!isSingularBlocked(initialdesign, vInv)) {
                candidateScore = calculateBlockedTOptimality(initialdesign, vInv);
                success = true;
              }
            } catch (std::runtime_error&) {
              success = false;
            }
            initialdesign.row(row) = originalRow;
            return success;
          },
          skpr::OptimizationSense::Maximize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow(row) = bestCandidate + 1;
          initialRows(row) = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow(row) = initialRows(row);
        }
      }
      newOptimum = calculateBlockedTOptimality(initialdesign, vInv);
    }
  }
  else if(condition == "E") {
    newOptimum = calculateBlockedEOptimality(initialdesign, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = calculateBlockedEOptimality(initialdesign, vInv);
      for (int row = 0; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            bool success = false;
            try {
              initialdesign.row(row) = candidatelist.row(candidateIndex);
              if(!isSingularBlocked(initialdesign, vInv)) {
                candidateScore = calculateBlockedEOptimality(initialdesign, vInv);
                success = true;
              }
            } catch (std::runtime_error&) {
              success = false;
            }
            initialdesign.row(row) = originalRow;
            return success;
          },
          skpr::OptimizationSense::Maximize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow(row) = bestCandidate + 1;
          initialRows(row) = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow(row) = initialRows(row);
        }
      }
      newOptimum = calculateBlockedEOptimality(initialdesign, vInv);
    }
  }
  else if(condition == "CUSTOM") {
    Environment myEnv = Environment::global_env();
    Function customBlockedOpt = myEnv["customBlockedOpt"];
    newOptimum = calculateBlockedCustomOptimality(initialdesign, customBlockedOpt, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = calculateBlockedCustomOptimality(initialdesign, customBlockedOpt, vInv);
      for (int row = 0; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            bool success = false;
            try {
              initialdesign.row(row) = candidatelist.row(candidateIndex);
              if(!isSingularBlocked(initialdesign, vInv)) {
                candidateScore = calculateBlockedCustomOptimality(initialdesign, customBlockedOpt, vInv);
                success = true;
              }
            } catch (std::runtime_error&) {
              success = false;
            }
            initialdesign.row(row) = originalRow;
            return success;
          },
          skpr::OptimizationSense::Maximize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow(row) = bestCandidate + 1;
          initialRows(row) = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow(row) = initialRows(row);
        }
      }
      newOptimum = calculateBlockedCustomOptimality(initialdesign, customBlockedOpt, vInv);
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["model_matrix"] = initialdesign, _["criterion"] = newOptimum));
}
