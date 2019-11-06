#include <RcppEigen.h>

Eigen::VectorXi sample_replace(int max_value, int size) {
  Eigen::VectorXi index(size);
  for (int i = 0; i < size; i++) {
    index(i) = max_value * unif_rand();
  }
  return(index);
}

//sample without replacement
Eigen::VectorXi sample_noreplace(int max_value, int size) {
  if(size > max_value) {
    throw std::range_error("argument `size` cannot be greater than `max_value` when sampling without replacment");
  }
  int i, j;
  Eigen::VectorXi index(size);
  Eigen::VectorXi sub(max_value);
  for (i = 0; i < max_value; i++) {
    sub(i) = i;
  }
  for (i = 0; i < size; i++) {
    j = max_value * unif_rand();
    index(i) = sub(j);
    sub(j) = sub(--max_value);
  }
  return(index);
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
  Eigen::VectorXi random_indices = sample_replace(nTrials, nTrials);
  for (int i = p; i < nTrials; i++) {
    design_rows(i) = random_indices(i);
  }

  return design_rows;
}

