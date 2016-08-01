// [[Rcpp::depends(RcppArmadillo)]]
#define ARMA_DONT_PRINT_ERRORS
#include <RcppArmadillo.h>
using namespace Rcpp;

double delta(arma::mat V, arma::mat x, arma::mat y) {
  return(as_scalar(-x*V*x.t() + y*V*y.t() + (y*V*x.t())*(y*V*x.t()) - (x*V*x.t())*(y*V*y.t())));
}

double calculateIOptimality(arma::mat currentDesign, const arma::mat momentsMatrix) {
  double variance = trace(inv_sympd(currentDesign.t()*currentDesign)*momentsMatrix);
  return(variance);
}

//Function to calculate the A-optimality
double calculateAOptimality(arma::mat currentDesign) {
  double variance = trace(inv_sympd(currentDesign.t()*currentDesign));
  return(variance);
}

//`@title genOptimalDesign
//`@param x an x
//`@return stufff
// [[Rcpp::export]]
List genOptimalDesign(arma::mat initialdesign, const arma::mat candidatelist,const std::string condition,
                      const arma::mat momentsmatrix, const NumericVector initialRows) {
  int check = 0;
  int nTrials = initialdesign.n_rows;
  int maxSingularityChecks = nTrials;
  int totalPoints = candidatelist.n_rows;
  IntegerVector candidateRow(nTrials);
  arma::mat test(initialdesign.n_cols,initialdesign.n_cols,arma::fill::zeros);
  if(nTrials <= candidatelist.n_cols) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  //Checks if the initial matrix is singular. If so, randomly generates a new design maxSingularityChecks times.
  if (!inv_sympd(test,initialdesign.t() * initialdesign)) {
    for(int j = 1; j < initialdesign.n_cols; j++) {
      if(all(initialdesign.col(0) == initialdesign.col(j))) {
        throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
      }
    }
    while(check < maxSingularityChecks) {
      if(!inv_sympd(test,initialdesign.t() * initialdesign)) {
        arma::vec randomrows = arma::randi<arma::vec>(nTrials, arma::distr_param(0, totalPoints-1));
        for(int i = 0; i < nTrials; i++) {
          initialdesign.row(i) = candidatelist.row(randomrows(i));
        }
        check++;
      } else {
        break;
      }
    }
    //If still no non-singular design, throws and error and exits.
    if (!inv_sympd(test,initialdesign.t() * initialdesign)) {
      throw std::runtime_error("All initial attempts to generate a non-singular matrix failed");
    }
  }
  bool found = FALSE;
  double del = 0;
  int entryx = 0;
  int entryy = 0;
  arma::mat V;
  double newdel;
  //Generate a D-optimal design
  if(condition == "D") {
    for (int i = 0; i < nTrials; i++) {
      found = FALSE;
      del = 0;
      entryx = 0;
      entryy = 0;
      V = inv_sympd(initialdesign.t() * initialdesign);
      for (int j = 0; j < totalPoints; j++) {
        newdel = delta(V,initialdesign.row(i),candidatelist.row(j));
        if(newdel > del) {
          found = TRUE;
          entryx = i; entryy = j;
          del = newdel;
        }
      }
      if (found) {
        initialdesign.row(entryx) = candidatelist.row(entryy);
        candidateRow[i] = entryy+1;
      } else {
        candidateRow[i] = initialRows[i];
      }
    }
  }
  //Generate an I-optimal design
  if(condition == "I") {
    arma::mat temp;
    double del = calculateIOptimality(initialdesign,momentsmatrix);
    for (int i = 0; i < nTrials; i++) {
      found = FALSE;
      entryx = 0;
      entryy = 0;
      temp = initialdesign;
      for (int j = 0; j < totalPoints; j++) {
        //Checks for singularity; If singular, moves to next candidate in the candidate set
        try {
          temp.row(i) = candidatelist.row(j);
          double newdel = calculateIOptimality(temp,momentsmatrix);
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
      } else {
        candidateRow[i] = initialRows[i];
      }
    }
  }
  //Generate an A-optimal design
  if(condition == "A") {
    arma::mat temp;
    double del = calculateAOptimality(initialdesign);
    for (int i = 0; i < nTrials; i++) {
      found = FALSE;
      entryx = 0;
      entryy = 0;
      temp = initialdesign;
      for (int j = 0; j < totalPoints; j++) {
        //Checks for singularity; If singular, moves to next candidate in the candidate set
        try {
          temp.row(i) = candidatelist.row(j);
          double newdel = calculateAOptimality(temp);
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
      } else {
        candidateRow[i] = initialRows[i];
      }
    }
  }
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = initialdesign));
}
