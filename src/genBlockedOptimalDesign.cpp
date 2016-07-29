// [[Rcpp::depends(RcppArmadillo)]]
#define ARMA_DONT_PRINT_ERRORS
#include <RcppArmadillo.h>
using namespace Rcpp;

double calculateBlockedDOptimality(arma::mat currentDesign) {
  return(arma::det(currentDesign.t()*currentDesign));
}

double calculateBlockedIOptimality(arma::mat currentDesign, const arma::mat momentsMatrix) {
  double variance = trace(inv_sympd(currentDesign.t()*currentDesign)*momentsMatrix);
  return(variance);
}

//Function to calculate the A-optimality
double calculateBlockedAOptimality(arma::mat currentDesign) {
  double variance = trace(inv_sympd(currentDesign.t()*currentDesign));
  return(variance);
}

//`@title genOptimalDesign
//`@param x an x
//`@return stufff
// [[Rcpp::export]]
List genBlockedOptimalDesign(arma::mat initialdesign, arma::mat candidatelist, const arma::mat blockeddesign,
                      const std::string condition, const arma::mat momentsmatrix, const NumericVector initialRows) {
  //Remove intercept term, as it's now located in the blocked
  candidatelist.shed_col(0);
  initialdesign.shed_col(0);

  int check = 0;
  int nTrials = initialdesign.n_rows;
  int maxSingularityChecks = nTrials;
  int totalPoints = candidatelist.n_rows;
  int blockedCols = blockeddesign.n_cols;
  int designCols = initialdesign.n_cols;
  arma::mat combinedDesign(nTrials,blockedCols+designCols,arma::fill::zeros);
  combinedDesign(arma::span::all,arma::span(0,blockedCols-1)) = blockeddesign;
  combinedDesign(arma::span::all,arma::span(blockedCols,blockedCols+designCols-1)) = initialdesign;
  IntegerVector candidateRow(nTrials);
  arma::mat test(combinedDesign.n_cols,combinedDesign.n_cols,arma::fill::zeros);
  if(nTrials <= candidatelist.n_cols + blockedCols) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  //Checks if the initial matrix is singular. If so, randomly generates a new design nTrials times.
  if (!inv_sympd(test,combinedDesign.t() * combinedDesign)) {
    for(int j = 1; j < blockedCols+designCols; j++) {
      if(all(combinedDesign.col(0) == combinedDesign.col(j))) {
        throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
      }
    }
    while(!inv_sympd(test,combinedDesign.t() * combinedDesign) && check < maxSingularityChecks) {
      arma::vec randomrows = arma::randi<arma::vec>(nTrials, arma::distr_param(0, totalPoints-1));
      for(int i = 0; i < nTrials; i++) {
        combinedDesign(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(randomrows(i));
      }
      check++;
    }
    //If still no non-singular design, throws and error and exits.
    if (!inv_sympd(test,combinedDesign.t() * combinedDesign)) {
      throw std::runtime_error("All initial attempts to generate a non-singular matrix failed");
    }
  }
  //Generate a D-optimal design, fixing the blocking factors
  if(condition == "D") {
    double del = calculateBlockedDOptimality(combinedDesign);
    for (int i = 0; i < nTrials; i++) {
      bool found = FALSE;
      int entryx = 0;
      int entryy = 0;
      arma::mat temp = combinedDesign;
      for (int j = 0; j < totalPoints; j++) {
        temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
        double newdel = calculateBlockedDOptimality(temp);
        if(newdel > del) {
          found = TRUE;
          entryx = i;
          entryy = j;
          del = newdel;
        }
      }
      if (found) {
        combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
        candidateRow[i] = entryy+1;
      } else {
        candidateRow[i] = initialRows[i];
      }
    }
  }
  //Generate an I-optimal design, fixing the blocking factors
  if(condition == "I") {
    //Potential issue here: Any interactions between the blocked and regular runs need to be
    //reflected in the moments matrix correctly
    double del = calculateBlockedIOptimality(combinedDesign,momentsmatrix);
    for (int i = 0; i < nTrials; i++) {
      bool found = FALSE;
      int entryx = 0;
      int entryy = 0;
      arma::mat temp = combinedDesign;
      for (int j = 0; j < totalPoints; j++) {
        //Checks for singularity; If singular, moves to next candidate in the candidate set
        try {
          temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
          double newdel = calculateBlockedIOptimality(temp,momentsmatrix);
          if(newdel < del) {
            found = TRUE;
            entryx = i;
            entryy = j;
            del = newdel;
          }
        } catch (std::runtime_error& e) {
          continue;
        }
      }
      if (found) {
        combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
        candidateRow[i] = entryy+1;
      } else {
        candidateRow[i] = initialRows[i];
      }
    }
  }
  //Generate an A-optimal design, fixing the blocking factors
  if(condition == "A") {
    double del = calculateBlockedAOptimality(combinedDesign);
    for (int i = 0; i < nTrials; i++) {
      bool found = FALSE;
      int entryx = 0;
      int entryy = 0;
      arma::mat temp = combinedDesign;
      for (int j = 0; j < totalPoints; j++) {
        //Checks for singularity; If singular, moves to next candidate in the candidate set
        try {
          temp(i,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(j);
          double newdel = calculateBlockedAOptimality(temp);
          if(newdel < del) {
            found = TRUE;
            entryx = i;
            entryy = j;
            del = newdel;
          }
        } catch (std::runtime_error& e) {
          continue;
        }
      }
      if (found) {
        combinedDesign(entryx,arma::span(blockedCols,blockedCols+designCols-1)) = candidatelist.row(entryy);
        candidateRow[i] = entryy+1;
      } else {
        candidateRow[i] = initialRows[i];
      }
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = combinedDesign));
}
