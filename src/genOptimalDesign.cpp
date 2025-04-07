#include <Rcpp.h>
#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]
#include <queue>
#include <string>

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
      return(List::create(_["indices"] = NumericVector::get_na(), _["model.matrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
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
    return(List::create(_["indices"] = NumericVector::get_na(), _["model.matrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
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
  return(List::create(_["indices"] = candidateRow, _["model.matrix"] = initialdesign, _["criterion"] = newOptimum));
}

