// [[Rcpp::depends(RcppArmadillo)]]
#define ARMA_DONT_PRINT_ERRORS
#include <RcppArmadillo.h>
using namespace Rcpp;

double delta(arma::mat V, arma::mat x, arma::mat y) {
  return(as_scalar(-x*V*x.t() + y*V*y.t() + (y*V*x.t())*(y*V*x.t()) - (x*V*x.t())*(y*V*y.t())));
}

double calculateDOptimality(arma::mat currentDesign) {
  return(arma::det(currentDesign.t()*currentDesign));
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
List genOptimalDesign(arma::mat initialdesign, arma::mat candidatelist,const std::string condition,
                      const arma::mat momentsmatrix, NumericVector initialRows, bool hasdisallowedcombinations) {
  unsigned int check = 0;
  unsigned int nTrials = initialdesign.n_rows;
  unsigned int maxSingularityChecks = nTrials*100;
  unsigned int totalPoints = candidatelist.n_rows;
  arma::vec candidateRow(nTrials);
  arma::mat test(initialdesign.n_cols,initialdesign.n_cols,arma::fill::zeros);
  //Check to see if the design could be a full factorial or replicated full factorial
  if(nTrials % totalPoints == 0 && !hasdisallowedcombinations) {
    double criterion;
    for(unsigned int i = 0; i < nTrials; i++) {
      initialdesign.row(i) = candidatelist.row(i % totalPoints);
      candidateRow[i] = i % totalPoints+1;
    }
    arma::vec returnRow = shuffle(candidateRow);
    if(condition == "D") {
      criterion = calculateDOptimality(initialdesign);
    }
    if(condition == "I") {
      criterion = calculateIOptimality(initialdesign,momentsmatrix);
    }
    if(condition == "A") {
      criterion = calculateAOptimality(initialdesign);
    }
    return(List::create(_["indices"] = returnRow, _["modelmatrix"] = initialdesign, _["criterion"] = criterion));
  }
  if(nTrials <= candidatelist.n_cols) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  for(unsigned int j = 1; j < candidatelist.n_cols; j++) {
    if(all(candidatelist.col(0) == candidatelist.col(j))) {
      throw std::runtime_error("Singular model matrix from factor aliased into intercept, revise model");
    }
  }
  //Checks if the initial matrix is singular. If so, randomly generates a new design maxSingularityChecks times.
  while(check < maxSingularityChecks) {
    if(!inv_sympd(test,initialdesign.t() * initialdesign)) {
      if (nTrials < totalPoints) {
        arma::vec shuffledindices = arma::shuffle(arma::regspace(0,totalPoints-1));
        arma::vec indices(nTrials);
        for(unsigned int i = 0; i < nTrials; i++) {
          indices(i) = shuffledindices(i);
        }
        for(unsigned int row = 0; row < nTrials; row++) {
          initialdesign.row(row) = candidatelist.row(indices(row));
        }
      } else {
        arma::vec randomrows = arma::randi<arma::vec>(nTrials, arma::distr_param(0, totalPoints-1));
        for(unsigned int i = 0; i < nTrials; i++) {
          initialdesign.row(i) = candidatelist.row(randomrows(i));
        }
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
  //return the model matrix and a list of the candidate list indices used to construct the run matrix
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = initialdesign, _["criterion"] = newOptimum));
}
