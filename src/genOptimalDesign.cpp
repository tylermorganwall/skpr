// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

double delta(arma::mat V, arma::mat x, arma::mat y) {
  return(as_scalar(-x*V*x.t() + y*V*y.t() + (y*V*x.t())*(y*V*x.t()) - (x*V*x.t())*(y*V*y.t())));
}

bool isSingular(arma::mat X) {
  try {
    //(X.t()*X).inv_sympd();
  }
  catch (exception& e){
    return(FALSE);
  }
  return(TRUE);
}
double calculateIOptimality(arma::mat currentDesign, const arma::mat momentsMatrix) {
  double variance = trace(inv_sympd(currentDesign.t()*currentDesign)*momentsMatrix);
  return(variance);
}

double calculateAOptimality(arma::mat currentDesign) {
  double variance = trace(inv_sympd(currentDesign.t()*currentDesign));
  return(variance);
}

//`@title genOptimalDesign
//`@param x an x
//`@return stufff
// [[Rcpp::export]]
List genOptimalDesign(arma::mat initialdesign, arma::mat candidatelist,const std::string condition,
                      const arma::mat momentsmatrix, const NumericVector initialRows) {
  int maxSingularityChecks = 10;
  int check = 0;
  int nTrials = initialdesign.n_rows;
  int totalPoints = candidatelist.n_rows;
  IntegerVector candidateRow(nTrials);
  arma::mat test(initialdesign.n_cols,initialdesign.n_cols,arma::fill::zeros);
  if(nTrials <= candidatelist.n_cols) {
    throw std::runtime_error("Too few runs to generate initial non-singular matrix: increase the number of runs or decrease the number of parameters in the matrix");
  }
  if (!inv_sympd(test,initialdesign.t() * initialdesign)) {
    std::ostream nullstream(0);
    arma::set_stream_err2(nullstream);
    while(!inv_sympd(test,initialdesign.t() * initialdesign) && check < maxSingularityChecks) {
      arma::vec randomrows = arma::randi<arma::vec>(nTrials, arma::distr_param(0, totalPoints-1));
      for(int i = 0; i < nTrials; i++) {
        initialdesign.row(i) = candidatelist.row(randomrows(i));
      }
      check++;
    }
    if (!inv_sympd(test,initialdesign.t() * initialdesign)) {
      throw std::runtime_error("All initial attempts to generate a non-singular matrix failed");
    }
  }
  if(condition == "D") {
    for (int i = 0; i < nTrials; i++) {
      bool found = FALSE;
      double del = 0;
      int entryx = 0;
      int entryy = 0;
      arma::mat V = inv_sympd(initialdesign.t() * initialdesign);
      for (int j = 0; j < totalPoints; j++) {
        double newdel = delta(V,initialdesign.row(i),candidatelist.row(j));
        if(newdel > del) {
          found = TRUE;
          entryx = i;
          entryy = j;
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
  if(condition == "I") {
    double del = calculateIOptimality(initialdesign,momentsmatrix);
    for (int i = 0; i < nTrials; i++) {
      bool found = FALSE;
      int entryx = 0;
      int entryy = 0;
      arma::mat temp = initialdesign;
      for (int j = 0; j < totalPoints; j++) {
        try {
          temp.row(i) = candidatelist.row(j);
          double newdel = calculateIOptimality(temp,momentsmatrix);
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
        initialdesign.row(entryx) = candidatelist.row(entryy);
        candidateRow[i] = entryy+1;
      } else {
        candidateRow[i] = initialRows[i];
      }
    }
  }
  if(condition == "A") {
    double del = calculateAOptimality(initialdesign);
    for (int i = 0; i < nTrials; i++) {
      bool found = FALSE;
      int entryx = 0;
      int entryy = 0;
      arma::mat temp = initialdesign;
      for (int j = 0; j < totalPoints; j++) {
        try {
          temp.row(i) = candidatelist.row(j);
          double newdel = calculateAOptimality(temp);
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
        initialdesign.row(entryx) = candidatelist.row(entryy);
        candidateRow[i] = entryy+1;
      } else {
        candidateRow[i] = initialRows[i];
      }
    }
  }
  return(List::create(_["indices"] = candidateRow, _["modelmatrix"] = initialdesign));
}
