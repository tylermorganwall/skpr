#define ARMA_DONT_PRINT_ERRORS
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//implements the Gram-Schmidt orthogonalization procdure to generate an initial non-singular design
arma::uvec orthogonal_initial(const arma::mat& candidatelist, unsigned int nTrials);

//helper functions for orthogonal_initial
unsigned int longest_row(const arma::mat& X, const std::vector<bool>& rows_used);
void orthogonalize_input(arma::mat& X, unsigned int basis_row, const std::vector<bool>& rows_used);


double delta(const arma::mat& V, const arma::mat& x, const arma::mat& y) {
  return(as_scalar(-x*V*x.t() + y*V*y.t() + (y*V*x.t())*(y*V*x.t()) - (x*V*x.t())*(y*V*y.t())));
}

double calculateDOptimality(const arma::mat& currentDesign) {
  return(arma::det(currentDesign.t()*currentDesign));
}

double calculateIOptimality(const arma::mat& currentDesign, const arma::mat& momentsMatrix) {
  return(trace(inv_sympd(currentDesign.t()*currentDesign)*momentsMatrix));
}


double calculateGOptimality(const arma::mat& currentDesign, const arma::mat& candidateSet) {
  arma::mat results = candidateSet*inv_sympd(currentDesign.t()*currentDesign)*candidateSet.t();
  return(results.diag().max());
}

double calculateTOptimality(const arma::mat& currentDesign) {
  return(trace(currentDesign.t()*currentDesign));
}

double calculateEOptimality(const arma::mat& currentDesign) {
  arma::vec eigval;
  arma::eig_sym(eigval,currentDesign.t()*currentDesign);
  return(eigval.min());
}

double calculateAOptimality(const arma::mat& currentDesign) {
  return(trace(inv_sympd(currentDesign.t()*currentDesign)));
}

double calculateAliasTrace(const arma::mat& currentDesign, const arma::mat& aliasMatrix) {
  arma::mat A = inv_sympd(currentDesign.t()*currentDesign)*currentDesign.t()*aliasMatrix;
  return(trace(A.t() * A));
}

double calculateAliasTracePseudoInv(const arma::mat& currentDesign, const arma::mat& aliasMatrix) {
  arma::mat A = arma::pinv(currentDesign.t()*currentDesign)*currentDesign.t()*aliasMatrix;
  return(trace(A.t() * A));
}


double calculateDEff(const arma::mat& currentDesign) {
  return(pow(arma::det(currentDesign.t()*currentDesign),(1.0/double(currentDesign.n_cols)))/double(currentDesign.n_rows));
}

double calculateDEffNN(const arma::mat& currentDesign) {
  return(pow(arma::det(currentDesign.t()*currentDesign),(1.0/double(currentDesign.n_cols))));
}

bool isSingular(const arma::mat& currentDesign) {
  return(arma::cond(currentDesign.t()*currentDesign) > 1E15);
}

template <typename T>
Rcpp::NumericVector arma2vec(const T& x) {
  return Rcpp::NumericVector(x.begin(), x.end());
}



//`@title genOptimalDesign
//`@param x an x
//`@return stufff
// [[Rcpp::export]]
List genOptimalDesign(arma::mat initialdesign, const arma::mat& candidatelist,const std::string condition,
                      const arma::mat& momentsmatrix, NumericVector initialRows,
                      arma::mat aliasdesign, const arma::mat& aliascandidatelist, double minDopt) {
  RNGScope rngScope;
  unsigned int nTrials = initialdesign.n_rows;
  unsigned int maxSingularityChecks = nTrials*100;
  unsigned int totalPoints = candidatelist.n_rows;
  arma::vec candidateRow(nTrials);
  arma::mat test(initialdesign.n_cols,initialdesign.n_cols,arma::fill::zeros);
  if(nTrials < candidatelist.n_cols) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  for(unsigned int j = 1; j < candidatelist.n_cols; j++) {
    if(all(candidatelist.col(0) == candidatelist.col(j))) {
      throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
    }
  }
  //Checks if the initial matrix is singular. If so, randomly generates a new design maxSingularityChecks times.
  for (unsigned int check = 0; check < maxSingularityChecks; check++) {
    if(!isSingular(initialdesign)) {
      break; //design is nonsingular
    }
    arma::uvec shuffledindices = RcppArmadillo::sample(arma::regspace<arma::uvec>(0, totalPoints-1), totalPoints, false);
    for (unsigned int i = 0; i < nTrials; i++) {
      initialdesign.row(i) = candidatelist.row(shuffledindices(i % totalPoints));
      aliasdesign.row(i) = aliascandidatelist.row(shuffledindices(i % totalPoints));
      initialRows(i) = shuffledindices(i % totalPoints) + 1; //R indexes start at 1
    }
  }
  //If initialdesign is still singular, use the Gram-Schmidt orthogonalization procedure, which
  //should return a non-singular matrix if one can be constructed from the candidate set
  if (isSingular(initialdesign)) {
    arma::uvec initrows = orthogonal_initial(candidatelist, nTrials);
    arma::uvec initrows_shuffled = RcppArmadillo::sample(initrows, initrows.n_rows, false);
    for (unsigned int i = 0; i < nTrials; i++) {
      initialdesign.row(i) = candidatelist.row(initrows_shuffled(i));
      aliasdesign.row(i) = aliascandidatelist.row(initrows_shuffled(i));
      initialRows(i) = initrows_shuffled(i) + 1; //R indexes start at 1
    }
  }

  //If still no non-singular design, returns NA.
  if (isSingular(initialdesign)) {
    return(List::create(_["indices"] = NumericVector::get_na(), _["modelmatrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
  }
  bool found = FALSE;
  double del = 0;
  int entryx = 0;
  int entryy = 0;
  double newOptimum = 0;
  double priorOptimum = 0;
  double minDelta = 10e-5;
  arma::mat V;
  double newdel;
  //Generate a D-optimal design
  if(condition == "D") {
    arma::mat temp;
    del = calculateDOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          temp.row(i) = candidatelist.row(j);
          newdel = calculateDOptimality(temp);
          if(newdel > del) {
            found = TRUE;
            entryx = i; entryy = j;
            del = newdel;
          }
        }
        if (found) {
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateDOptimality(initialdesign);
    }
  }
  //Generate an I-optimal design
  if(condition == "I") {
    arma::mat temp;
    del = calculateIOptimality(initialdesign,momentsmatrix);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.row(i) = candidatelist.row(j);
            newdel = calculateIOptimality(temp,momentsmatrix);
            if(newdel < del) {
              found = TRUE;
              entryx = i; entryy = j;
              del = newdel;
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateIOptimality(initialdesign,momentsmatrix);
    }
  }
  //Generate an A-optimal design
  if(condition == "A") {
    arma::mat temp;
    del = calculateAOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.row(i) = candidatelist.row(j);
            newdel = calculateAOptimality(temp);
            if(newdel < del) {
              found = TRUE;
              entryx = i; entryy = j;
              del = newdel;
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateAOptimality(initialdesign);
    }
  }
  //Generate an Alias optimal design
  if(condition == "Alias") {

    arma::mat temp;
    arma::mat tempalias;
    del = calculateDOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;

    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          temp.row(i) = candidatelist.row(j);
          newdel = calculateDOptimality(temp);
          if(newdel > del) {
            found = TRUE;
            entryx = i; entryy = j;
            del = newdel;
          }
        }
        if (found) {
          initialdesign.row(entryx) = candidatelist.row(entryy);
          aliasdesign.row(entryx) = aliascandidatelist.row(entryy);

          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateDOptimality(initialdesign);
    }

    double firstA = calculateAliasTracePseudoInv(initialdesign,aliasdesign);
    double initialD = calculateDEffNN(initialdesign);
    double currentA = firstA;
    double currentD = initialD;
    double wdelta = 0.05;
    double aliasweight = 1;
    double bestA = firstA;
    double optimum;
    double first = 1;

    arma::vec candidateRowTemp = candidateRow;
    arma::vec initialRowsTemp = initialRows;
    arma::mat initialdesignTemp = initialdesign;

    arma::vec bestcandidaterow = candidateRowTemp;
    arma::mat bestaliasdesign = aliasdesign;
    arma::mat bestinitialdesign = initialdesign;

    while(firstA != 0 && currentA != 0 && aliasweight > wdelta) {

      aliasweight = aliasweight - wdelta;
      optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      first = 1;

      while((optimum - priorOptimum)/priorOptimum > minDelta || first == 1) {
        first++;
        priorOptimum = optimum;
        for (unsigned int i = 0; i < nTrials; i++) {
          found = FALSE;
          entryx = 0;
          entryy = 0;
          temp = initialdesignTemp;
          tempalias = aliasdesign;
          for (unsigned int j = 0; j < totalPoints; j++) {
            try {
              temp.row(i) = candidatelist.row(j);
              tempalias.row(i) = aliascandidatelist.row(j);
              currentA = calculateAliasTrace(temp,tempalias);
              currentD = calculateDEffNN(temp);

              newdel = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);

              if(newdel > optimum && calculateDEff(temp) > minDopt) {
                found = TRUE;
                entryx = i; entryy = j;
                optimum = newdel;
              }
            } catch (std::runtime_error& e) {
              continue;
            }
          }
          if (found) {
            initialdesignTemp.row(entryx) = candidatelist.row(entryy);
            aliasdesign.row(entryx) = aliascandidatelist.row(entryy);
            candidateRowTemp[i] = entryy+1;
            initialRowsTemp[i] = entryy+1;
          } else {
            candidateRowTemp[i] = initialRowsTemp[i];
          }
        }
        currentD = calculateDEffNN(initialdesignTemp);
        currentA = calculateAliasTrace(initialdesignTemp,aliasdesign);
        optimum = aliasweight*currentD/initialD + (1-aliasweight)*(1-currentA/firstA);
      }

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
    arma::mat temp;
    del = calculateGOptimality(initialdesign,candidatelist);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp.row(i) = candidatelist.row(j);
            newdel = calculateGOptimality(temp,candidatelist);
            if(newdel < del) {
              if(!isSingular(temp)) {
                found = TRUE;
                entryx = i; entryy = j;
                del = newdel;
              }
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateGOptimality(initialdesign,candidatelist);
    }
  }
  if(condition == "T") {
    arma::mat temp;
    del = calculateTOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          temp.row(i) = candidatelist.row(j);
          newdel = calculateTOptimality(temp);
          if(newdel > del) {
            if(!isSingular(temp)) {
              found = TRUE;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateTOptimality(initialdesign);
    }
  }
  if(condition == "E") {
    arma::mat temp;
    del = calculateEOptimality(initialdesign);
    newOptimum = del;
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = initialdesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          temp.row(i) = candidatelist.row(j);
          newdel = calculateEOptimality(temp);
          if(newdel > del) {
            if(!isSingular(temp)) {
              found = TRUE;
              entryx = i; entryy = j;
              del = newdel;
            }
          }
        }
        if (found) {
          initialdesign.row(entryx) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateEOptimality(initialdesign);
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = initialdesign, _["criterion"] = newOptimum));
}

//**********************************************************
//Everything below is for generating blocked optimal designs
//**********************************************************

double calculateBlockedDOptimality(const arma::mat& currentDesign, const arma::mat& gls) {
  return(arma::det(currentDesign.t()*gls*currentDesign));
}

double calculateBlockedIOptimality(const arma::mat& currentDesign, const arma::mat& momentsMatrix,const arma::mat& gls) {
  return(trace(inv_sympd(currentDesign.t()*gls*currentDesign)*momentsMatrix));
}

//Function to calculate the A-optimality
double calculateBlockedAOptimality(const arma::mat& currentDesign,const arma::mat& gls) {
  return(trace(inv_sympd(currentDesign.t()*gls*currentDesign)));
}

double calculateBlockedAliasTrace(const arma::mat& currentDesign, const arma::mat& aliasMatrix,const arma::mat& gls) {
  arma::mat A = inv_sympd(currentDesign.t()*gls*currentDesign)*currentDesign.t()*aliasMatrix;
  return(trace(A.t() * A));
}

double calculateBlockedGOptimality(const arma::mat& currentDesign, const arma::mat& candidateSet, const arma::mat& gls) {
  arma::mat results = candidateSet*inv_sympd(currentDesign.t()*gls*currentDesign)*candidateSet.t();
  return(results.diag().max());
}

double calculateBlockedTOptimality(const arma::mat& currentDesign,const arma::mat& gls) {
  return(trace(currentDesign.t()*gls*currentDesign));
}

double calculateBlockedEOptimality(const arma::mat& currentDesign,const arma::mat& gls) {
  arma::vec eigval;
  arma::eig_sym(eigval,currentDesign.t()*gls*currentDesign);
  return(eigval.min());
}

double calculateBlockedDEff(const arma::mat& currentDesign,const arma::mat& gls) {
  return(pow(arma::det(currentDesign.t()*gls*currentDesign),(1.0/double(currentDesign.n_cols)))/double(currentDesign.n_rows));
}

double calculateBlockedDEffNN(const arma::mat& currentDesign,const arma::mat& gls) {
  return(pow(arma::det(currentDesign.t()*gls*currentDesign),(1.0/double(currentDesign.n_cols))));
}

//`@title genBlockedOptimalDesign
//`@param x an x
//`@return stufff
// [[Rcpp::export]]
List genBlockedOptimalDesign(arma::mat initialdesign, arma::mat candidatelist, const arma::mat& blockeddesign,
                             const std::string condition, const arma::mat& momentsmatrix, IntegerVector initialRows,
                             const arma::mat& blockedVar,
                             const arma::mat& aliasdesign, const arma::mat& aliascandidatelist, double minDopt, List interactions,
                             const arma::mat& disallowed, const bool anydisallowed) {
  //Load the R RNG
  RNGScope rngScope;

  //check and log whether there are inter-strata interactions
  unsigned int numberinteractions = interactions.size();
  bool interstrata = (numberinteractions > 0);
  //Generate blocking structure inverse covariance matrix
  const arma::mat vInv = inv_sympd(blockedVar);
  //Checks if the initial matrix is singular. If so, randomly generates a new design nTrials times.
  for(unsigned int j = 1; j < candidatelist.n_cols; j++) {
    if(all(candidatelist.col(0) == candidatelist.col(j))) {
      throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
    }
  }
  //Remove intercept term, as it's now located in the blocked
  candidatelist.shed_col(0);
  initialdesign.shed_col(0);

  unsigned int nTrials = initialdesign.n_rows;
  unsigned int maxSingularityChecks = nTrials*10;
  unsigned int totalPoints = candidatelist.n_rows;
  unsigned int blockedCols = blockeddesign.n_cols;
  int designCols = initialdesign.n_cols;
  LogicalVector mustchange(nTrials, false);
  //arma::mat combinedDesign(nTrials,blockedCols+designCols,arma::fill::zeros);
  arma::mat combinedDesign(nTrials,blockedCols+designCols + numberinteractions,arma::fill::zeros);
  combinedDesign(arma::span::all,arma::span(0,blockedCols-1)) = blockeddesign;
  combinedDesign(arma::span::all,arma::span(blockedCols,blockedCols+designCols-1)) = initialdesign;

  if(interstrata) {
    for(unsigned int i = 0; i < numberinteractions; i++) {
      combinedDesign.col(blockedCols+designCols + i) = combinedDesign.col(as<NumericVector>(interactions[i])[0]-1) % combinedDesign.col(as<NumericVector>(interactions[i])[1]-1);
    }
  }

  IntegerVector candidateRow = initialRows;
  arma::mat test(combinedDesign.n_cols,combinedDesign.n_cols,arma::fill::zeros);
  if(nTrials < candidatelist.n_cols + blockedCols + numberinteractions) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  for (unsigned int check = 0; check < maxSingularityChecks; check++) {
    if (inv(test, combinedDesign.t() * vInv * combinedDesign)){
      break;
    }
    arma::uvec shuffledindices = RcppArmadillo::sample(arma::regspace<arma::uvec>(0, totalPoints-1), totalPoints, false);
    for (unsigned int i = 0; i < nTrials; i++) {
      candidateRow(i) = shuffledindices(i % totalPoints) + 1;
      initialRows(i) = shuffledindices(i % totalPoints) + 1;
      combinedDesign(i, arma::span(blockedCols, blockedCols+designCols-1)) = candidatelist.row(shuffledindices(i % totalPoints));
      if (interstrata) {
        for(unsigned int j = 0; j < numberinteractions; j++) {
          combinedDesign(i, blockedCols+designCols + j) = combinedDesign(i, as<NumericVector>(interactions[j])[0]-1) * combinedDesign(i, as<NumericVector>(interactions[j])[1]-1);
        }
      }
    }
  }
  // If still no non-singular design, returns NA.
  if (!inv(test,combinedDesign.t() * vInv * combinedDesign)) {
    return(List::create(_["indices"] = NumericVector::get_na(), _["modelmatrix"] = NumericMatrix::get_na(), _["criterion"] = NumericVector::get_na()));
  }
  if(anydisallowed) {
    for(unsigned int i = 0; i < nTrials; i++) {
      for(unsigned int j = 0; j < disallowed.n_rows; j++) {
        if(all(combinedDesign.row(i) == disallowed.row(j))) {
          mustchange[i] = true;
        }
      }
    }
  }
  double del = 0;
  bool found = FALSE;
  int entryx = 0;
  int entryy = 0;
  double newOptimum = 0;
  double priorOptimum = 0;
  double minDelta = 10e-5;
  double newdel;
  bool pointallowed = false;
  //Generate a D-optimal design, fixing the blocking factors
  if(condition == "D") {
    arma::mat temp;
    newOptimum = calculateBlockedDOptimality(combinedDesign, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedDOptimality(combinedDesign,vInv);
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;

        for (unsigned int j = 0; j < totalPoints; j++) {
          temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);

          if(interstrata) {
            for(unsigned int k = 0; k < numberinteractions; k++) {
              temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
            }
          }

          pointallowed = true;
          if(anydisallowed) {
            for(unsigned int k = 0; k < disallowed.n_rows; k++) {
              if(all(temp.row(i) == disallowed.row(k))) {
                pointallowed = false;
              }
            }
          }

          newdel = calculateBlockedDOptimality(temp, vInv);
          if((newdel > del || mustchange[i]) && pointallowed) {
            found = TRUE;
            entryx = i; entryy = j;
            del = newdel;
            mustchange[i] = false;
          }
        }
        if (found) {
          combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
          if(interstrata) {
            for(unsigned int k = 0; k < numberinteractions; k++) {
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
    arma::mat temp;
    del = calculateBlockedIOptimality(combinedDesign,momentsmatrix,vInv);
    newOptimum = del;
    priorOptimum = del/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
            if(interstrata) {
              for(unsigned int k = 0; k < numberinteractions; k++) {
                temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
              }
            }

            pointallowed = true;
            if(anydisallowed) {
              for(unsigned int k = 0; k < disallowed.n_rows; k++) {
                if(all(temp.row(i) == disallowed.row(k))) {
                  pointallowed = false;
                }
              }
            }

            newdel = calculateBlockedIOptimality(temp,momentsmatrix,vInv);
            if((newdel > del || mustchange[i]) && pointallowed) {
              found = TRUE;
              entryx = i; entryy = j;
              del = newdel;
              mustchange[i] = false;
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
          if(interstrata) {
            for(unsigned int k = 0; k < numberinteractions; k++) {
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
    arma::mat temp;
    del = calculateBlockedAOptimality(combinedDesign,vInv);
    newOptimum = del;
    priorOptimum = del*2;
    while((newOptimum - priorOptimum)/priorOptimum < -minDelta) {
      priorOptimum = newOptimum;
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          //Checks for singularity; If singular, moves to next candidate in the candidate set
          try {
            temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
            if(interstrata) {
              for(unsigned int k = 0; k < numberinteractions; k++) {
                temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
              }
            }
            pointallowed = true;
            if(anydisallowed) {
              for(unsigned int k = 0; k < disallowed.n_rows; k++) {
                if(all(temp.row(i) == disallowed.row(k))) {
                  pointallowed = false;
                }
              }
            }

            newdel = calculateBlockedAOptimality(temp,vInv);
            if((newdel > del || mustchange[i]) && pointallowed) {
              found = TRUE;
              entryx = i; entryy = j;
              del = newdel;
              mustchange[i] = false;
            }
          } catch (std::runtime_error& e) {
            continue;
          }
        }
        if (found) {
          combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
          if(interstrata) {
            for(unsigned int k = 0; k < numberinteractions; k++) {
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
  //Commented out until bugs worked out
  /*if(condition == "G") {
  arma::mat temp;
  newOptimum = calculateBlockedGOptimality(combinedDesign, candidatelist, vInv);
  priorOptimum = newOptimum/2;
  while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
  priorOptimum = newOptimum;
  del = calculateBlockedGOptimality(combinedDesign,candidatelist,vInv);
  for (unsigned int i = 0; i < nTrials; i++) {
  found = FALSE;
  entryx = 0;
  entryy = 0;
  temp = combinedDesign;
  for (unsigned int j = 0; j < totalPoints; j++) {
  temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
  newdel = calculateBlockedGOptimality(temp, candidatelist, vInv);
  if(newdel > del) {
  found = TRUE;
  entryx = i; entryy = j;
  del = newdel;
  }
  }
  if (found) {
  combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
  candidateRow[i] = entryy+1;
  initialRows[i] = entryy+1;
  } else {
  candidateRow[i] = initialRows[i];
  }
  }
  newOptimum = calculateBlockedGOptimality(combinedDesign, candidatelist, vInv);
  }
}
  if(condition == "T") {
  arma::mat temp;
  newOptimum = calculateBlockedTOptimality(combinedDesign, vInv);
  priorOptimum = newOptimum/2;
  while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
  priorOptimum = newOptimum;
  del = calculateBlockedTOptimality(combinedDesign,vInv);
  for (unsigned int i = 0; i < nTrials; i++) {
  found = FALSE;
  entryx = 0;
  entryy = 0;
  temp = combinedDesign;
  for (unsigned int j = 0; j < totalPoints; j++) {
  temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
  newdel = calculateBlockedTOptimality(temp, vInv);
  if(newdel > del) {
  found = TRUE;
  entryx = i; entryy = j;
  del = newdel;
  }
  }
  if (found) {
  combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
  candidateRow[i] = entryy+1;
  initialRows[i] = entryy+1;
  } else {
  candidateRow[i] = initialRows[i];
  }
  }
  newOptimum = calculateBlockedTOptimality(combinedDesign, vInv);
  }
  }
  */
  //Generate an E-optimal design, fixing the blocking factors
  if(condition == "E") {
    arma::mat temp;
    newOptimum = calculateBlockedEOptimality(combinedDesign, vInv);
    priorOptimum = newOptimum/2;
    while((newOptimum - priorOptimum)/priorOptimum > minDelta) {
      priorOptimum = newOptimum;
      del = calculateBlockedEOptimality(combinedDesign,vInv);
      for (unsigned int i = 0; i < nTrials; i++) {
        found = FALSE;
        entryx = 0;
        entryy = 0;
        temp = combinedDesign;
        for (unsigned int j = 0; j < totalPoints; j++) {
          temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
          if(interstrata) {
            for(unsigned int k = 0; k < numberinteractions; k++) {
              temp(i,blockedCols+designCols + k) = temp(i,as<NumericVector>(interactions[k])[0]-1) * temp(i,as<NumericVector>(interactions[k])[1]-1);
            }
          }

          pointallowed = true;
          if(anydisallowed) {
            for(unsigned int k = 0; k < disallowed.n_rows; k++) {
              if(all(temp.row(i) == disallowed.row(k))) {
                pointallowed = false;
              }
            }
          }

          newdel = calculateBlockedEOptimality(temp, vInv);
          if((newdel > del || mustchange[i]) && pointallowed) {
            found = TRUE;
            entryx = i; entryy = j;
            del = newdel;
            mustchange[i] = false;
          }
        }
        if (found) {
          combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
          if(interstrata) {
            for(unsigned int k = 0; k < numberinteractions; k++) {
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
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = combinedDesign, _["criterion"] = newOptimum));
}

arma::uvec orthogonal_initial(const arma::mat& candidatelist, unsigned int nTrials) {
  //Construct a nonsingular design matrix from candidatelist using the nullify procedure
  //Returns a vector of rownumbers indicating which runs from candidatelist to use
  //These rownumbers are not shuffled; you must do that yourself if randomizing the order is important
  //If we cannot find a nonsingular design, returns a vector of zeros.

  //First, find the p rows that come from the nullify procedure:
  //    find the longest row vector in the candidatelist
  //    orthogonalize the rest of the candidatelist to this vector
  arma::mat candidatelist2(candidatelist); //local copy we will orthogonalize
  std::vector<bool> design_flag(candidatelist2.n_rows, false); //indicates that a candidate row has been used in the design
  arma::uvec design_rows(nTrials);  //return value

  double tolerance = 1e-8;
  const unsigned int p = candidatelist2.n_cols;
  for (unsigned int i = 0; i < p; i++) {
    unsigned int nextrow = longest_row(candidatelist2, design_flag);
    double nextrow_length = arma::norm(candidatelist2.row(nextrow));
    if (i == 0) {
      tolerance = tolerance * nextrow_length; //scale tolerance to candidate list's longest vector
    }
    if (nextrow_length < tolerance) {
      return arma::uvec(nTrials, arma::fill::zeros); //rank-deficient candidate list, return error state
    }
    design_flag[nextrow] = true;
    design_rows[i] = nextrow;
    if (i != (p-1)) {
      orthogonalize_input(candidatelist2, nextrow, design_flag);
    }
  }
  //Then fill in the design with N - p randomly chosen rows from the candidatelist
  arma::uvec random_indices = RcppArmadillo::sample(arma::regspace<arma::uvec>(0, candidatelist2.n_rows-1), nTrials, true);
  for (unsigned int i = p; i < nTrials; i++) {
    design_rows(i) = random_indices(i);
  }

  return design_rows;
}


unsigned int longest_row(const arma::mat& V, const std::vector<bool>& rows_used) {
  //Return the index of the longest unused row in V
  double longest = -1;
  unsigned int index = 0;
  for (unsigned int i = 0; i < V.n_rows; i++) {
    if (!rows_used[i]) {
      double this_len = arma::dot(V.row(i), V.row(i));
      if (this_len > longest) {
        longest = this_len;
        index = i;
      }
    }
  }
  return index;
}


void orthogonalize_input(arma::mat& X, unsigned int basis_row, const std::vector<bool>& rows_used) {
  //Gram-Schmidt orthogonalize <X> - in place - with respect to its rownumber <basis_row>
  //Only unused rows (as indicated by <rows_used>) are considered.
  double basis_norm = arma::dot(X.row(basis_row), X.row(basis_row));
  for (unsigned int i = 0; i < X.n_rows; i++) {
    if (!rows_used[i]) {
      double dotprod = arma::dot(X.row(i), X.row(basis_row));
      X.row(i) -= X.row(basis_row)*dotprod/basis_norm;
    }
  }
}

