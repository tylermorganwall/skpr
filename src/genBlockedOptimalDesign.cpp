// [[Rcpp::depends(RcppArmadillo)]]
#define ARMA_DONT_PRINT_ERRORS
#include <RcppArmadillo.h>
using namespace Rcpp;

double calculateBlockedDOptimality(const arma::mat currentDesign, const arma::mat gls) {
  return(arma::det(currentDesign.t()*gls*currentDesign));
}

double calculateBlockedIOptimality(const arma::mat currentDesign, const arma::mat momentsMatrix,const arma::mat gls) {
  return(trace(inv_sympd(currentDesign.t()*gls*currentDesign)*momentsMatrix));
}

//Function to calculate the A-optimality
double calculateBlockedAOptimality(arma::mat currentDesign,const arma::mat gls) {
  return(trace(inv_sympd(currentDesign.t()*gls*currentDesign)));
}

double calculateBlockedAliasTrace(arma::mat currentDesign, arma::mat aliasMatrix) {
  arma::mat A = inv_sympd(currentDesign.t()*currentDesign)*currentDesign.t()*aliasMatrix;
  return(trace(A.t() * A));
}

double calculateBlockedDEff(arma::mat currentDesign) {
  return(pow(arma::det(currentDesign.t()*currentDesign),(1.0/double(currentDesign.n_cols)))/double(currentDesign.n_rows));
}

//`@title genOptimalDesign
//`@param x an x
//`@return stufff
// [[Rcpp::export]]
List genBlockedOptimalDesign(arma::mat initialdesign, arma::mat candidatelist, const arma::mat blockeddesign,
                      const std::string condition, const arma::mat momentsmatrix, NumericVector initialRows,
                      const arma::mat blockedVar,
                      arma::mat aliasdesign, arma::mat aliascandidatelist, double minDopt) {

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

  unsigned int check = 0;
  unsigned int nTrials = initialdesign.n_rows;
  unsigned int maxSingularityChecks = nTrials*100;
  unsigned int totalPoints = candidatelist.n_rows;
  unsigned int blockedCols = blockeddesign.n_cols;
  int designCols = initialdesign.n_cols;
  arma::mat combinedDesign(nTrials,blockedCols+designCols,arma::fill::zeros);
  combinedDesign(arma::span::all,arma::span(0,blockedCols-1)) = blockeddesign;
  combinedDesign(arma::span::all,arma::span(blockedCols,blockedCols+designCols-1)) = initialdesign;
  IntegerVector candidateRow(nTrials);
  arma::mat test(combinedDesign.n_cols,combinedDesign.n_cols,arma::fill::zeros);
  if(nTrials <= candidatelist.n_cols + blockedCols) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  while(!inv_sympd(test,combinedDesign.t() * combinedDesign) && check < maxSingularityChecks) {
    if (nTrials < totalPoints) {
      arma::vec shuffledindices = arma::shuffle(arma::regspace(0,totalPoints-1));
      arma::vec indices(nTrials);
      for(unsigned int i = 0; i < nTrials; i++) {
        indices(i) = shuffledindices(i);
      }
      for(unsigned int i = 0; i < nTrials; i++) {
        combinedDesign(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(indices(i));
      }
    } else {
      arma::vec randomrows = arma::randi<arma::vec>(nTrials, arma::distr_param(0, totalPoints-1));
      for(unsigned int i = 0; i < nTrials; i++) {
        combinedDesign(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(randomrows(i));
      }
    }
    check++;
  }
  //If still no non-singular design, throws and error and exits.
  if (!inv_sympd(test,combinedDesign.t() * combinedDesign)) {
    throw std::runtime_error("All initial attempts to generate a non-singular matrix failed");
  }
  double del = 0;
  bool found = FALSE;
  int entryx = 0;
  int entryy = 0;
  double newOptimum = 0;
  double priorOptimum = 0;
  double minDelta = 10e-5;
  double newdel;
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
          newdel = calculateBlockedDOptimality(temp, vInv);
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
      newOptimum = calculateBlockedDOptimality(combinedDesign, vInv);
    }
  }
  //Generate an I-optimal design, fixing the blocking factors
  if(condition == "I") {
    //Potential issue here: Any interactions between the blocked and regular runs need to be
    //reflected in the moments matrix correctly
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
            newdel = calculateBlockedIOptimality(temp,momentsmatrix,vInv);
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
          combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedIOptimality(combinedDesign,momentsmatrix,vInv);
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
            newdel = calculateBlockedAOptimality(temp,vInv);
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
          combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
          candidateRow[i] = entryy+1;
          initialRows[i] = entryy+1;
        } else {
          candidateRow[i] = initialRows[i];
        }
      }
      newOptimum = calculateBlockedAOptimality(combinedDesign,vInv);
    }
  }

  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = combinedDesign, _["criterion"] = newOptimum));
}
