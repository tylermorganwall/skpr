#include <RcppEigen.h>

using namespace Rcpp;

//implements the Gram-Schmidt orthogonalization procdure to generate an initial non-singular design
Eigen::VectorXi orthogonal_initial(const Eigen::MatrixXd& candidatelist, int nTrials);

Eigen::VectorXi sample_replace(Eigen::VectorXi index, int number_sampled, int size) {
  for (int i = 0; i < size; i++) {
    index(i) = number_sampled * unif_rand();
  }
  return(index);
}

Eigen::VectorXi sample_noreplace(Eigen::VectorXi index, int number_sampled, int size) {
  int i, j;
  Eigen::VectorXi sub(number_sampled);
  for (i = 0; i < number_sampled; i++) {
    sub(i) = i;
  }
  for (i = 0; i < size; i++) {
    j = number_sampled * unif_rand();
    index(i) = sub(j);
    sub(j) = sub(--number_sampled);
  }
  return(index);
}

//helper functions for orthogonal_initial
int longest_row(const Eigen::MatrixXd& X, const std::vector<bool>& rows_used);
void orthogonalize_input(Eigen::MatrixXd& X, int basis_row, const std::vector<bool>& rows_used);

void search_candidate_set(const Eigen::MatrixXd& V, const Eigen::MatrixXd& candidatelist_trans,
                          const Eigen::VectorXd& designrow,
                          double xVx, int& entryy, bool& found, double& del) {
  Eigen::VectorXd yV(candidatelist_trans.rows());
  double newdel = 0;
  int ncols = candidatelist_trans.cols();
  for (int j = 0; j < ncols; j++) {
    yV = V * candidatelist_trans.col(j);
    newdel = yV.dot(candidatelist_trans.col(j))*(1 - xVx) - xVx + pow(yV.dot(designrow),2);
    if(newdel > del) {
      found = true;
      entryy = j;
      del = newdel;
    }
  }
}


double calculateDOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(XtX.partialPivLu().determinant());  //works without partialPivLu()
}

double calculateIOptimality(const Eigen::MatrixXd& currentV, const Eigen::MatrixXd& momentsMatrix) {
  return((currentV*momentsMatrix).trace());
}


double calculateGOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& candidateSet) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  Eigen::MatrixXd results = candidateSet*XtX.llt().solve(candidateSet.transpose());
  return(results.diagonal().maxCoeff());
}

double calculateTOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(XtX.trace());
}

double calculateEOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensolver(XtX);
  return(eigensolver.eigenvalues().minCoeff());
}

double calculateAOptimality(const Eigen::MatrixXd& currentV) {
  return(currentV.trace());
}

double calculateAliasTraceSlow(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  Eigen::MatrixXd A = XtX.llt().solve(currentDesign.transpose())*aliasMatrix;
  return((A.transpose() * A).trace());
}

double calculateAliasTrace(const Eigen::MatrixXd& currentV,
                           const Eigen::MatrixXd& currentDesign,
                           const Eigen::MatrixXd& aliasMatrix) {
  Eigen::MatrixXd A = currentV*currentDesign.transpose()*aliasMatrix;
  return((A.transpose() * A).trace());
}

double calculateAliasTracePseudoInv(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix) {
  //since we use linear solving, I see no need for this function
  return(calculateAliasTraceSlow(currentDesign, aliasMatrix));
}

double calculateDEff(const Eigen::MatrixXd& currentDesign, double numbercols, double numberrows) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1/numbercols) / numberrows);
}

double calculateDEffNN(const Eigen::MatrixXd& currentDesign, double numbercols) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1.0/numbercols));
}

bool isSingular(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*currentDesign;
  return(!XtX.colPivHouseholderQr().isInvertible());
}

template <typename T>
Rcpp::NumericVector arma2vec(const T& x) {
  return Rcpp::NumericVector(x.begin(), x.end());
}

double calculateCustomOptimality(const Eigen::MatrixXd& currentDesign, Function customOpt) {
  return as<double>(customOpt(Rcpp::Named("currentDesign", currentDesign)));
}

void rankUpdate(Eigen::MatrixXd& vinv, const Eigen::VectorXd& pointold, const Eigen::VectorXd& pointnew,
                const Eigen::MatrixXd& identity,
                Eigen::MatrixXd& f1, Eigen::MatrixXd& f2,Eigen::MatrixXd& f2vinv) {
  f1.col(0) = pointnew; f1.col(1) = -pointold;
  f2.col(0) = pointnew; f2.col(1) = pointold;
  f2vinv = f2.transpose()*vinv;
  Eigen::MatrixXd tmp = vinv - vinv * f1 * (identity + f2vinv*f1).partialPivLu().solve(f2vinv);
  vinv = tmp;
}

Eigen::MatrixXd rankUpdateValue(Eigen::MatrixXd& vinv, const Eigen::VectorXd& pointold, const Eigen::VectorXd& pointnew,
                                const Eigen::MatrixXd& identity,
                                Eigen::MatrixXd& f1, Eigen::MatrixXd& f2,Eigen::MatrixXd& f2vinv) {
  f1.col(0) = pointnew; f1.col(1) = -pointold;
  f2.col(0) = pointnew; f2.col(1) = pointold;
  f2vinv = f2.transpose()*vinv;
  Eigen::MatrixXd tmp = vinv - vinv * f1 * (identity + f2vinv*f1).partialPivLu().solve(f2vinv);
  return(tmp);
}


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
//`@return List of design information.
// [[Rcpp::export]]
List genOptimalDesign(Eigen::MatrixXd initialdesign, const Eigen::MatrixXd& candidatelist,const std::string condition,
                      const Eigen::MatrixXd& momentsmatrix, NumericVector initialRows,
                      Eigen::MatrixXd aliasdesign, const Eigen::MatrixXd& aliascandidatelist, double minDopt, double tolerance) {
  RNGScope rngScope;
  int nTrials = initialdesign.rows();
  double numberrows = initialdesign.rows();
  double numbercols = initialdesign.cols();
  int maxSingularityChecks = nTrials*100;
   int totalPoints = candidatelist.rows();
  Eigen::VectorXd candidateRow(nTrials);
  Eigen::MatrixXd test(initialdesign.cols(), initialdesign.cols());
  test.setZero();
  if(nTrials < candidatelist.cols()) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  for(int j = 1; j < candidatelist.cols(); j++) {
    if(candidatelist.col(0).cwiseEqual(candidatelist.col(j)).all()) {
      throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
    }
  }
  //Checks if the initial matrix is singular. If so, randomly generates a new design maxSingularityChecks times.
  for (int check = 0; check < maxSingularityChecks; check++) {
    if(!isSingular(initialdesign)) {
      break; //design is nonsingular
    }
    Eigen::VectorXi orderedindices = Eigen::VectorXi::LinSpaced(totalPoints, 0, totalPoints-1);
    Eigen::VectorXi shuffledindices = sample_noreplace(orderedindices, totalPoints, false);

    for (int i = 0; i < nTrials; i++) {
      initialdesign.row(i) = candidatelist.row(shuffledindices(i % totalPoints));
      aliasdesign.row(i) = aliascandidatelist.row(shuffledindices(i % totalPoints));
      initialRows(i) = shuffledindices(i % totalPoints) + 1; //R indexes start at 1
    }
  }
  //If initialdesign is still singular, use the Gram-Schmidt orthogonalization procedure, which
  //should return a non-singular matrix if one can be constructed from the candidate set
  if (isSingular(initialdesign)) {
    Eigen::VectorXi initrows = orthogonal_initial(candidatelist, nTrials);
    Eigen::VectorXi initrows_shuffled = sample_noreplace(initrows, initrows.rows(), false);
    for (int i = 0; i < nTrials; i++) {
      initialdesign.row(i) = candidatelist.row(initrows_shuffled(i));
      aliasdesign.row(i) = aliascandidatelist.row(initrows_shuffled(i));
      initialRows(i) = initrows_shuffled(i) + 1; //R indexes start at 1
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

  //Transpose matrices for faster element access (Armadillo stores data column-wise)
  Eigen::MatrixXd initialdesign_trans = initialdesign.transpose();
  Eigen::MatrixXd candidatelist_trans = candidatelist.transpose();
  Eigen::MatrixXd V = (initialdesign.transpose()*initialdesign).partialPivLu().inverse();

  //Generate a D-optimal design
  if(condition == "D" || condition == "G") {
    newOptimum = calculateDOptimality(initialdesign);
    priorOptimum = newOptimum/2;

    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
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
  }
  //Generate an I-optimal design
  if(condition == "I") {
    del = calculateIOptimality(V,momentsmatrix);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
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
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
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
      for (int i = 0; i < nTrials; i++) {
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

    double firstA = calculateAliasTracePseudoInv(initialdesign,aliasdesign);
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
        for (int i = 0; i < nTrials; i++) {
          Rcpp::checkUserInterrupt();
          found = false;
          entryx = 0;
          entryy = 0;
          temp = initialdesignTemp;
          tempalias = aliasdesign;
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
    del = calculateGOptimality(initialdesign,candidatelist);
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
        for (int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.row(i) = candidatelist.row(j);
            newdel = calculateGOptimality(temp,candidatelist);
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
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      //Re-calculate current criterion value.
      newOptimum = calculateGOptimality(initialdesign,candidatelist);
    }
  }
  if(condition == "T") {
    Eigen::MatrixXd temp;
    del = calculateTOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
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
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
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
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
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

double calculateBlockedDOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& gls) {
  return((currentDesign.transpose()*gls*currentDesign).partialPivLu().determinant());
}

double calculateBlockedIOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& momentsMatrix,const Eigen::MatrixXd& gls) {
  return(((currentDesign.transpose()*gls*currentDesign).llt().solve(momentsMatrix)).trace());
}

double calculateBlockedAOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  return((currentDesign.transpose()*gls*currentDesign).partialPivLu().inverse().trace());
}

double calculateBlockedAliasTrace(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  Eigen::MatrixXd A = XtX.llt().solve(currentDesign.transpose()*aliasMatrix);
  return((A.transpose() * A).trace());
}

// double calculateBlockedGOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& candidateSet, const Eigen::MatrixXd& gls) {
//   Eigen::MatrixXd results = inv_sympd(currentDesign.t()*gls*currentDesign)*candidateSet.t()*gls;
//   return(results.diag().max());
// }

double calculateBlockedTOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  return((currentDesign.transpose()*gls*currentDesign).trace());
}

double calculateBlockedEOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensolver(XtX);
  return(eigensolver.eigenvalues().minCoeff());
}

double calculateBlockedDEff(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1/currentDesign.cols()) / currentDesign.rows());
}

double calculateBlockedDEffNN(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  return(pow(XtX.partialPivLu().determinant(), 1.0/currentDesign.cols()));
}

double calculateBlockedAliasTracePseudoInv(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  Eigen::MatrixXd A = XtX.partialPivLu().solve(currentDesign.transpose()*aliasMatrix);
  return((A.transpose() * A).trace());
}

bool isSingularBlocked(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls) {
  Eigen::MatrixXd XtX = currentDesign.transpose()*gls*currentDesign;
  return(!XtX.colPivHouseholderQr().isInvertible());
}

double calculateBlockedCustomOptimality(const Eigen::MatrixXd& currentDesign, Function customBlockedOpt, const Eigen::MatrixXd& gls) {
  return as<double>(customBlockedOpt(Rcpp::Named("currentDesign", currentDesign),Rcpp::Named("vInv", gls)));
}


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
List genBlockedOptimalDesign(Eigen::MatrixXd initialdesign, Eigen::MatrixXd candidatelist, const Eigen::MatrixXd& blockeddesign,
                             const std::string condition, const Eigen::MatrixXd& momentsmatrix, IntegerVector initialRows,
                             const Eigen::MatrixXd& blockedVar,
                             Eigen::MatrixXd aliasdesign, Eigen::MatrixXd aliascandidatelist, double minDopt, List interactions,
                             const Eigen::MatrixXd disallowed, const bool anydisallowed, double tolerance) {
  //Load the R RNG
  RNGScope rngScope;
  //check and log whether there are inter-strata interactions
  int numberinteractions = interactions.size();
  bool interstrata = (numberinteractions > 0);
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
  //Eigen::MatrixXd combinedAliasDesign(nTrials,blockedCols+ designColsAlias + numberinteractions,arma::fill::zeros);
  //combinedAliasDesign(arma::span::all,arma::span(0,blockedCols-1)) = blockeddesign;
  //combinedAliasDesign(arma::span::all,arma::span(blockedCols,blockedCols+designColsAlias-1)) = aliasdesign;

  if(interstrata) {
    for(int i = 0; i < numberinteractions; i++) {
      combinedDesign.col(blockedCols+designCols + i) = combinedDesign.col(as<NumericVector>(interactions[i])[0]-1).cwiseProduct(combinedDesign.col(as<NumericVector>(interactions[i])[1]-1));
      combinedAliasDesign.col(blockedCols+designColsAlias + i) = combinedAliasDesign.col(as<NumericVector>(interactions[i])[0]-1).cwiseProduct(combinedAliasDesign.col(as<NumericVector>(interactions[i])[1]-1));
    }
  }

  IntegerVector candidateRow = initialRows;
  Eigen::MatrixXd test(combinedDesign.cols(),combinedDesign.cols());
  test.setZero();
  if(nTrials < candidatelist.cols() + blockedCols + numberinteractions) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  for (int check = 0; check < maxSingularityChecks; check++) {
    if (!isSingularBlocked(combinedDesign,vInv)){
      break;
    }
    Eigen::VectorXi ordered_indices = Eigen::VectorXi::LinSpaced(totalPoints, 0, totalPoints-1);
    Eigen::VectorXi shuffledindices = sample_noreplace(ordered_indices, totalPoints, false);

    for (int i = 0; i < nTrials; i++) {
      candidateRow(i) = shuffledindices(i % totalPoints) + 1;
      initialRows(i) = shuffledindices(i % totalPoints) + 1;
      combinedDesign.block(i, blockedCols, 1, designCols) = candidatelist.row(shuffledindices(i % totalPoints));
      combinedAliasDesign.block(i, blockedCols, 1, designColsAlias) = aliascandidatelist.row(shuffledindices(i % totalPoints));
      if (interstrata) {
        for(int j = 0; j < numberinteractions; j++) {
          combinedDesign.col(blockedCols+designCols + j) = combinedDesign.col(as<NumericVector>(interactions[j])[0]-1).cwiseProduct(combinedDesign.col(as<NumericVector>(interactions[j])[1]-1));
          combinedAliasDesign.col(blockedCols+designColsAlias + j) = combinedAliasDesign.col(as<NumericVector>(interactions[j])[0]-1).cwiseProduct(combinedAliasDesign.col(as<NumericVector>(interactions[j])[1]-1));
        }
      }
    }
  }
  // If still no non-singular design, returns NA.
  if (isSingularBlocked(combinedDesign,vInv)) {
    return(List::create(_["indices"] = NumericVector::get_na(), _["modelmatrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
  }
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
  bool pointallowed = false;
  //Generate a D-optimal design, fixing the blocking factors
  if(condition == "D") {
    Eigen::MatrixXd temp;
    newOptimum = calculateBlockedDOptimality(combinedDesign, vInv);
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
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
            }
          }

          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }
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
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              combinedDesign(i,blockedCols+designCols + k) = combinedDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesign(i,as<NumericVector>(interactions[k])[1]-1);
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
  }
  //Generate an I-optimal design, fixing the blocking factors
  if(condition == "I") {
    Eigen::MatrixXd temp;
    del = calculateBlockedIOptimality(combinedDesign,momentsmatrix,vInv);
    newOptimum = del;
    priorOptimum = del/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        for (int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
            if(interstrata) {
              for(int k = 0; k < numberinteractions; k++) {
                temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
              }
            }

            pointallowed = true;
            if(anydisallowed) {
              for(int k = 0; k < disallowed.rows(); k++) {
                if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                  pointallowed = false;
                }
              }
            }

            newdel = calculateBlockedIOptimality(temp,momentsmatrix,vInv);
            if((newdel > del || mustchange[i]) && pointallowed) {
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
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              combinedDesign(i,blockedCols+designCols + k) = combinedDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesign(i,as<NumericVector>(interactions[k])[1]-1);
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
        for (int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
            if(interstrata) {
              for(int k = 0; k < numberinteractions; k++) {
                temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
              }
            }
            pointallowed = true;
            if(anydisallowed) {
              for(int k = 0; k < disallowed.rows(); k++) {
                if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                  pointallowed = false;
                }
              }
            }

            newdel = calculateBlockedAOptimality(temp,vInv);
            if((newdel > del || mustchange[i]) && pointallowed) {
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
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              combinedDesign(i,blockedCols+designCols + k) = combinedDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesign(i,as<NumericVector>(interactions[k])[1]-1);
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
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
            }
          }

          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }

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
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              combinedDesign(i,blockedCols+designCols + k) = combinedDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesign(i,as<NumericVector>(interactions[k])[1]-1);
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
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
            }
          }

          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }
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
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              combinedDesign(i,blockedCols+designCols + k) = combinedDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesign(i,as<NumericVector>(interactions[k])[1]-1);
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

  // Work-in-progress
  // if(condition == "G") {
  //
  //   Eigen::MatrixXd reducedCandidateList(fullcandidatesetrows(), blockedCols + designCols + numberinteractions,arma::fill::zeros);
  //   reducedCandidateList(arma::span::all,arma::span(0,blockedCols+designCols-1)) = fullcandidateset;
  //
  //   if(interstrata) {
  //     for(int i = 0; i < numberinteractions; i++) {
  //       reducedCandidateList.col(blockedCols+designCols + i) = reducedCandidateList.col(as<NumericVector>(interactions[i])[0]-1) % reducedCandidateList.col(as<NumericVector>(interactions[i])[1]-1);
  //     }
  //   }
  //
  //   LogicalVector mustdelete(fullcandidatesetrows(), false);
  //
  //   if(anydisallowed) {
  //     for(int i = 0; i < fullcandidatesetrows(); i++) {
  //       for(int j = 0; j < disallowedrows(); j++) {
  //         if(all(reducedCandidateList.row(i) == disallowed.row(j))) {
  //           mustdelete[i] = true;
  //         }
  //       }
  //     }
  //     for(int i = fullcandidatesetrows(); i > 0; i--) {
  //       if(mustdelete[i]) {
  //         reducedCandidateList.shed_row(i);
  //       }
  //     }
  //   }
  //
  //   Eigen::MatrixXd temp;
  //   newOptimum = calculateBlockedGOptimality(combinedDesign, reducedCandidateList, vInv);
  //   priorOptimum = newOptimum*2;
  //   while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
  //     del = newOptimum;
  //     priorOptimum = newOptimum;
  //     for (int i = 0; i < nTrials; i++) {
  //       Rcpp::checkUserInterrupt();
  //       found = false;
  //       entryx = 0;
  //       entryy = 0;
  //       temp = combinedDesign;
  //       for (int j = 0; j < totalPoints; j++) {
  //         temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
  //         if(interstrata) {
  //           for(int k = 0; k < numberinteractions; k++) {
  //             temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
  //           }
  //         }
  //
  //         pointallowed = true;
  //         if(anydisallowed) {
  //           for(int k = 0; k < disallowedrows(); k++) {
  //             if(all(temp.row(i) == disallowed.row(k))) {
  //               pointallowed = false;
  //             }
  //           }
  //         }
  //         try {
  //           newdel = calculateBlockedGOptimality(temp, reducedCandidateList, vInv);
  //         } catch (std::runtime_error& e) {
  //           continue;
  //         }
  //         if((newdel < del || mustchange[i]) && pointallowed) {
  //           if(!isSingularBlocked(temp,vInv)) {
  //             found = true;
  //             entryx = i; entryy = j;
  //             del = newdel;
  //             mustchange[i] = false;
  //           }
  //         }
  //       }
  //       if (found) {
  //         combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
  //         if(interstrata) {
  //           for(int k = 0; k < numberinteractions; k++) {
  //             combinedDesign(i,blockedCols+designCols + k) = combinedDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesign(i,as<NumericVector>(interactions[k])[1]-1);
  //           }
  //         }
  //         candidateRow[i] = entryy+1;
  //         initialRows[i] = entryy+1;
  //       } else {
  //         candidateRow[i] = initialRows[i];
  //       }
  //     }
  //     newOptimum = calculateBlockedGOptimality(combinedDesign, reducedCandidateList, vInv);
  //   }
  // }

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

        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);

          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
            }
          }

          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }

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
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              combinedDesign(i,blockedCols+designCols + k) = combinedDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesign(i,as<NumericVector>(interactions[k])[1]-1);
              combinedAliasDesign(i,blockedCols+designColsAlias + k) = combinedAliasDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedAliasDesign(i,as<NumericVector>(interactions[k])[1]-1);
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
          for (int j = 0; j < totalPoints; j++) {
            try {
              temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
              tempalias.block(i, blockedCols, 1, designColsAlias) = aliascandidatelist.row(j);
              if(interstrata) {
                for(int k = 0; k < numberinteractions; k++) {
                  temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
                  tempalias(i,blockedCols+designColsAlias + k) = tempalias(i,as<NumericVector>(interactions[k])[0]-1) * tempalias(i,as<NumericVector>(interactions[k])[1]-1);
                }
              }

              currentA = calculateBlockedAliasTrace(temp,tempalias,vInv);
              currentD = calculateBlockedDEffNN(temp,vInv);

              pointallowed = true;
              if(anydisallowed) {
                for(int k = 0; k < disallowed.rows(); k++) {
                  if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                    pointallowed = false;
                  }
                }
              }

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
            if(interstrata) {
              for(int k = 0; k < numberinteractions; k++) {
                combinedDesignTemp(i,blockedCols+designCols + k) = combinedDesignTemp(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesignTemp(i,as<NumericVector>(interactions[k])[1]-1);
                combinedAliasDesign(i,blockedCols+designColsAlias + k) = combinedAliasDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedAliasDesign(i,as<NumericVector>(interactions[k])[1]-1);
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
      for (int i = 0; i < nTrials; i++) {
        Rcpp::checkUserInterrupt();
        found = false;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        for (int j = 0; j < totalPoints; j++) {
          temp.block(i, blockedCols, 1, designCols) = candidatelist.row(j);
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
            }
          }

          pointallowed = true;
          if(anydisallowed) {
            for(int k = 0; k < disallowed.rows(); k++) {
              if(temp.row(i).cwiseEqual(disallowed.row(k)).all()) {
                pointallowed = false;
              }
            }
          }
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
          if(interstrata) {
            for(int k = 0; k < numberinteractions; k++) {
              combinedDesign(i,blockedCols+designCols + k) = combinedDesign(i,as<NumericVector>(interactions[k])[0]-1) * combinedDesign(i,as<NumericVector>(interactions[k])[1]-1);
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

Eigen::VectorXi orthogonal_initial(const Eigen::MatrixXd& candidatelist, int nTrials) {
  //Construct a nonsingular design matrix from candidatelist using the nullify procedure
  //Returns a vector of rownumbers indicating which runs from candidatelist to use
  //These rownumbers are not shuffled; you must do that yourself if randomizing the order is important
  //If we cannot find a nonsingular design, returns a vector of zeros.

  //First, find the p rows that come from the nullify procedure:
  //    find the longest row vector in the candidatelist
  //    orthogonalize the rest of the candidatelist to this vector
  Eigen::MatrixXd candidatelist2(candidatelist); //local copy we will orthogonalize
  std::vector<bool> design_flag(candidatelist2.rows(), false); //indicates that a candidate row has been used in the design
  Eigen::VectorXi design_rows(nTrials);  //return value

  double tolerance = 1e-8;
  const int p = candidatelist2.cols();
  for (int i = 0; i < p; i++) {
    int nextrow = longest_row(candidatelist2, design_flag);
    double nextrow_length = candidatelist2.row(nextrow).norm();
    if (i == 0) {
      tolerance = tolerance * nextrow_length; //scale tolerance to candidate list's longest vector
    }
    if (nextrow_length < tolerance) {
      return Eigen::VectorXi::Zero(nTrials, 1); //rank-deficient candidate list, return error state
    }
    design_flag[nextrow] = true;
    design_rows[i] = nextrow;
    if (i != (p-1)) {
      orthogonalize_input(candidatelist2, nextrow, design_flag);
    }
  }
  //Then fill in the design with N - p randomly chosen rows from the candidatelist
  Eigen::VectorXi ordered_indices = Eigen::VectorXi::LinSpaced(candidatelist2.rows(), 0, candidatelist2.rows()-1);
  Eigen::VectorXi random_indices = sample_replace(ordered_indices, nTrials, true);
  for (int i = p; i < nTrials; i++) {
    design_rows(i) = random_indices(i);
  }

  return design_rows;
}


int longest_row(const Eigen::MatrixXd& V, const std::vector<bool>& rows_used) {
  //Return the index of the longest unused row in V
  double longest = -1;
  int index = 0;
  for (int i = 0; i < V.rows(); i++) {
    if (!rows_used[i]) {
      double this_len = V.row(i).dot(V.row(i));
      if (this_len > longest) {
        longest = this_len;
        index = i;
      }
    }
  }
  return index;
}


void orthogonalize_input(Eigen::MatrixXd& X, int basis_row, const std::vector<bool>& rows_used) {
  //Gram-Schmidt orthogonalize <X> - in place - with respect to its rownumber <basis_row>
  //Only unused rows (as indicated by <rows_used>) are considered.
  double basis_norm = X.row(basis_row).dot(X.row(basis_row));
  for (int i = 0; i < X.rows(); i++) {
    if (!rows_used[i]) {
      double dotprod = X.row(i).dot(X.row(basis_row));
      X.row(i) -= X.row(basis_row)*dotprod/basis_norm;
    }
  }
}

