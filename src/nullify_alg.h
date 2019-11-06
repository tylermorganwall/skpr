#include <RcppEigen.h>

Eigen::VectorXi sample_replace(int max_value, int size);

Eigen::VectorXi sample_noreplace(int max_value, int size);

int longest_row(const Eigen::MatrixXd& V, const std::vector<bool>& rows_used);

void orthogonalize_input(Eigen::MatrixXd& X, int basis_row, const std::vector<bool>& rows_used);

Eigen::VectorXi orthogonal_initial(const Eigen::MatrixXd& candidatelist, int nTrials);
