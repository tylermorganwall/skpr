#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]
#include <queue>
#include <string>

#include "optimalityfunctions.h"
#include "nullify_alg.h"
#include "design_utils.h"

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
Rcpp::List genOptimalDesign(Eigen::MatrixXd initialdesign, const Eigen::MatrixXd& candidatelist,
                            const std::string condition,
                            const Eigen::MatrixXd& momentsmatrix, Eigen::VectorXd initialRows,
                            Eigen::MatrixXd aliasdesign,
                            const Eigen::MatrixXd& aliascandidatelist,
                            double minDopt, double tolerance, int augmentedrows, int kexchange) {
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
  if (isSingular(initialdesign)) {
    return(List::create(_["indices"] = NumericVector::get_na(), _["model_matrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
  }

  double del = 0.0;
  double newOptimum = 0.0;
  double priorOptimum = 0.0;
  const double minDelta = tolerance;

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
    newOptimum = 1.0;
    priorOptimum = newOptimum/2;

    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      //Calculate k-exchange coordinates
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
          q.emplace(temp_val, i);
        }
      } else {
        for (int i = augmentedrows; i < nTrials; i++) {
          q.emplace(-i, i);
        }
      }

      for (int j = 0; j < k && !q.empty(); j++) {
        Rcpp::checkUserInterrupt();
        const int columnIndex = q.top().second;
        q.pop();
        bool found = false;
        int entryy = 0;
        double gain = 0.0;
        const double xVx = initialdesign_trans.col(columnIndex).transpose() * V * initialdesign_trans.col(columnIndex);

        search_candidate_set(V, candidatelist_trans, initialdesign_trans.col(columnIndex), xVx, entryy, found, gain);

        if (found) {
          rankUpdate(V, initialdesign_trans.col(columnIndex), candidatelist_trans.col(entryy), identitymat, f1, f2, f2vinv);
          initialdesign_trans.col(columnIndex) = candidatelist_trans.col(entryy);
          candidateRow[columnIndex] = entryy + 1;
          initialRows[columnIndex] = entryy + 1;
          newOptimum = newOptimum * (1 + gain);
        } else {
          candidateRow[columnIndex] = initialRows[columnIndex];
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
  else if(condition == "I") {
    del = calculateIOptimality(V, momentsmatrix);
    newOptimum = del;
    priorOptimum = del * 2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = del;
      for (int columnIndex = augmentedrows; columnIndex < nTrials; ++columnIndex) {
        Rcpp::checkUserInterrupt();
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            candidateScore = calculateIOptimality(
              rankUpdateValue(V, initialdesign_trans.col(columnIndex), candidatelist_trans.col(candidateIndex),
                              identitymat, f1, f2, f2vinv),
              momentsmatrix);
            return true;
          },
          skpr::OptimizationSense::Minimize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          rankUpdate(V, initialdesign_trans.col(columnIndex), candidatelist_trans.col(bestCandidate), identitymat, f1, f2, f2vinv);
          initialdesign_trans.col(columnIndex) = candidatelist_trans.col(bestCandidate);
          candidateRow[columnIndex] = bestCandidate + 1;
          initialRows[columnIndex] = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow[columnIndex] = initialRows[columnIndex];
        }
      }
      initialdesign = initialdesign_trans.transpose();
      newOptimum = calculateIOptimality(V, momentsmatrix);
      del = newOptimum;
    }
  }
  //Generate an A-optimal design
  else if(condition == "A") {
    del = calculateAOptimality(V);
    newOptimum = del;
    priorOptimum = del * 2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = del;
      for (int columnIndex = augmentedrows; columnIndex < nTrials; ++columnIndex) {
        Rcpp::checkUserInterrupt();
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            candidateScore = calculateAOptimality(
              rankUpdateValue(V, initialdesign_trans.col(columnIndex), candidatelist_trans.col(candidateIndex),
                              identitymat, f1, f2, f2vinv));
            return true;
          },
          skpr::OptimizationSense::Minimize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          rankUpdate(V, initialdesign_trans.col(columnIndex), candidatelist_trans.col(bestCandidate), identitymat, f1, f2, f2vinv);
          initialdesign_trans.col(columnIndex) = candidatelist_trans.col(bestCandidate);
          candidateRow[columnIndex] = bestCandidate + 1;
          initialRows[columnIndex] = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow[columnIndex] = initialRows[columnIndex];
        }
      }
      initialdesign = initialdesign_trans.transpose();
      newOptimum = calculateAOptimality(V);
      del = newOptimum;
    }
  }
  //Generate an Alias optimal design
  if(condition == "ALIAS") {
    //First, calculate a D-optimal design (only do one iteration--may be only locally optimal) to start the search.
    del = calculateDOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;

    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = augmentedrows; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        bool found = false;
        int bestIndex = 0;
        double gain = 0.0;
        const double xVx = initialdesign_trans.col(i).transpose() * V * initialdesign_trans.col(i);

        search_candidate_set(V, candidatelist_trans, initialdesign_trans.col(i), xVx, bestIndex, found, gain);

        if (found) {
          rankUpdate(V, initialdesign_trans.col(i), candidatelist_trans.col(bestIndex), identitymat, f1, f2, f2vinv);
          initialdesign_trans.col(i) = candidatelist_trans.col(bestIndex);
          aliasdesign.row(i) = aliascandidatelist.row(bestIndex);
          candidateRow[i] = bestIndex + 1;
          initialRows[i] = bestIndex + 1;
          newOptimum = newOptimum * (1 + gain);
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
    Eigen::VectorXd initialRowsTemp = initialRows;
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
        for (int columnIndex = augmentedrows; columnIndex < nTrials; ++columnIndex) {
          Rcpp::checkUserInterrupt();
          const Eigen::RowVectorXd originalDesignRow = initialdesignTemp.row(columnIndex);
          const Eigen::RowVectorXd originalAliasRow = aliasdesign.row(columnIndex);
          double bestOptimum = optimum;

          const int bestCandidate = skpr::findBestCandidate(
            totalPoints,
            [&](int candidateIndex, double& candidateScore) -> bool {
              try {
                initialdesignTemp.row(columnIndex) = candidatelist.row(candidateIndex);
                aliasdesign.row(columnIndex) = aliascandidatelist.row(candidateIndex);
                Eigen::MatrixXd updatedV = rankUpdateValue(
                  V, initialdesign_trans.col(columnIndex), candidatelist_trans.col(candidateIndex),
                  identitymat, f1, f2, f2vinv);
                double candidateA = calculateAliasTrace(updatedV, initialdesignTemp, aliasdesign);
                double candidateD = calculateDEffNN(initialdesignTemp, numbercols);
                if(calculateDEff(initialdesignTemp, numbercols, numberrows) <= minDopt) {
                  initialdesignTemp.row(columnIndex) = originalDesignRow;
                  aliasdesign.row(columnIndex) = originalAliasRow;
                  return false;
                }
                candidateScore = aliasweight*candidateD/initialD + (1-aliasweight)*(1-candidateA/firstA);
              } catch (std::runtime_error&) {
                initialdesignTemp.row(columnIndex) = originalDesignRow;
                aliasdesign.row(columnIndex) = originalAliasRow;
                return false;
              }
              initialdesignTemp.row(columnIndex) = originalDesignRow;
              aliasdesign.row(columnIndex) = originalAliasRow;
              return true;
            },
            skpr::OptimizationSense::Maximize,
            optimum,
            bestOptimum
          );

          if (bestCandidate != -1) {
            rankUpdate(V, initialdesign_trans.col(columnIndex), candidatelist_trans.col(bestCandidate), identitymat, f1, f2, f2vinv);
            initialdesign_trans.col(columnIndex) = candidatelist_trans.col(bestCandidate);
            initialdesignTemp.row(columnIndex) = candidatelist.row(bestCandidate);
            aliasdesign.row(columnIndex) = aliascandidatelist.row(bestCandidate);
            candidateRowTemp[columnIndex] = bestCandidate + 1;
            initialRowsTemp[columnIndex] = bestCandidate + 1;
            optimum = bestOptimum;
          } else {
            initialdesignTemp.row(columnIndex) = originalDesignRow;
            aliasdesign.row(columnIndex) = originalAliasRow;
            candidateRowTemp[columnIndex] = initialRowsTemp[columnIndex];
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
  else if(condition == "G") {
    del = calculateGOptimality(V, initialdesign);
    newOptimum = del;
    priorOptimum = del * 2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = del;
      for (int row = augmentedrows; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            bool success = false;
            try {
              Eigen::MatrixXd updatedV = rankUpdateValue(
                V, initialdesign_trans.col(row), candidatelist_trans.col(candidateIndex),
                identitymat, f1, f2, f2vinv);
              initialdesign.row(row) = candidatelist.row(candidateIndex);
              if(!isSingular(initialdesign)) {
                candidateScore = calculateGOptimality(updatedV, initialdesign);
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
          rankUpdate(V, initialdesign_trans.col(row), candidatelist_trans.col(bestCandidate), identitymat, f1, f2, f2vinv);
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          initialdesign_trans.col(row) = candidatelist_trans.col(bestCandidate);
          candidateRow[row] = bestCandidate + 1;
          initialRows[row] = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow[row] = initialRows[row];
        }
      }
      newOptimum = calculateGOptimality(V, initialdesign);
      del = newOptimum;
    }
  }
  else if(condition == "T") {
    del = calculateTOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum / 2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = del;
      for (int row = augmentedrows; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            initialdesign.row(row) = candidatelist.row(candidateIndex);
            if(isSingular(initialdesign)) {
              initialdesign.row(row) = originalRow;
              return false;
            }
            candidateScore = calculateTOptimality(initialdesign);
            initialdesign.row(row) = originalRow;
            return true;
          },
          skpr::OptimizationSense::Maximize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow[row] = bestCandidate + 1;
          initialRows[row] = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow[row] = initialRows[row];
          initialdesign.row(row) = originalRow;
        }
      }
      newOptimum = calculateTOptimality(initialdesign);
      del = newOptimum;
    }
  }
  else if(condition == "E") {
    del = calculateEOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum / 2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = del;
      for (int row = augmentedrows; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            initialdesign.row(row) = candidatelist.row(candidateIndex);
            if(isSingular(initialdesign)) {
              initialdesign.row(row) = originalRow;
              return false;
            }
            candidateScore = calculateEOptimality(initialdesign);
            initialdesign.row(row) = originalRow;
            return true;
          },
          skpr::OptimizationSense::Maximize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow[row] = bestCandidate + 1;
          initialRows[row] = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow[row] = initialRows[row];
          initialdesign.row(row) = originalRow;
        }
      }
      newOptimum = calculateEOptimality(initialdesign);
      del = newOptimum;
    }
  }
  else if(condition == "CUSTOM") {
    Environment myEnv = Environment::global_env();
    Function customOpt = myEnv["customOpt"];
    del = calculateCustomOptimality(initialdesign, customOpt);
    newOptimum = del;
    priorOptimum = newOptimum / 2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      double currentCriterion = del;
      for (int row = augmentedrows; row < nTrials; ++row) {
        Rcpp::checkUserInterrupt();
        const Eigen::RowVectorXd originalRow = initialdesign.row(row);
        double bestCriterion = currentCriterion;

        const int bestCandidate = skpr::findBestCandidate(
          totalPoints,
          [&](int candidateIndex, double& candidateScore) -> bool {
            initialdesign.row(row) = candidatelist.row(candidateIndex);
            if(isSingular(initialdesign)) {
              initialdesign.row(row) = originalRow;
              return false;
            }
            candidateScore = calculateCustomOptimality(initialdesign, customOpt);
            initialdesign.row(row) = originalRow;
            return true;
          },
          skpr::OptimizationSense::Maximize,
          currentCriterion,
          bestCriterion
        );

        if (bestCandidate != -1) {
          initialdesign.row(row) = candidatelist.row(bestCandidate);
          candidateRow[row] = bestCandidate + 1;
          initialRows[row] = bestCandidate + 1;
          currentCriterion = bestCriterion;
        } else {
          candidateRow[row] = initialRows[row];
          initialdesign.row(row) = originalRow;
        }
      }
      newOptimum = calculateCustomOptimality(initialdesign, customOpt);
      del = newOptimum;
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["model_matrix"] = initialdesign, _["criterion"] = newOptimum));
}
