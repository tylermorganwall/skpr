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


//`@title optimalDesign
//`@param x an x
//`@return stufff
// [[Rcpp::export]]
arma::mat optimalDesign(arma::mat initialdesign, arma::mat candidatelist,const std::string condition, arma::mat momentsmatrix) {
  int nTrials = initialdesign.n_rows;
  int totalPoints = candidatelist.n_rows;
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
        temp.row(i) = candidatelist.row(j);
        double newdel = calculateIOptimality(temp,momentsmatrix);
        if(newdel < del) {
          found = TRUE;
          entryx = i;
          entryy = j;
          del = newdel;
        }
      }
      if (found) {
        initialdesign.row(entryx) = candidatelist.row(entryy);
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
        temp.row(i) = candidatelist.row(j);
        double newdel = calculateAOptimality(temp);
        if(newdel < del) {
          found = TRUE;
          entryx = i;
          entryy = j;
          del = newdel;
        }
      }
      if (found) {
        initialdesign.row(entryx) = candidatelist.row(entryy);
      }
    }
  }

  return(initialdesign);
}
