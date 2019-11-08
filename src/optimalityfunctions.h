#include <RcppEigen.h>

double calculateDOptimality(const Eigen::MatrixXd& currentDesign);

double calculateDOptimalityLog(const Eigen::MatrixXd& currentDesign);


double calculateIOptimality(const Eigen::MatrixXd& currentV, const Eigen::MatrixXd& momentsMatrix);

double calculateGOptimality(const Eigen::MatrixXd& currentV, const Eigen::MatrixXd& currentDesign);

double calculateTOptimality(const Eigen::MatrixXd& currentDesign);

double calculateEOptimality(const Eigen::MatrixXd& currentDesign);

double calculateAOptimality(const Eigen::MatrixXd& currentV);

double calculateAliasTraceSlow(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix);

double calculateAliasTrace(const Eigen::MatrixXd& currentV,
                           const Eigen::MatrixXd& currentDesign,
                           const Eigen::MatrixXd& aliasMatrix);

double calculateDEff(const Eigen::MatrixXd& currentDesign, double numbercols, double numberrows);

double calculateDEffLog(const Eigen::MatrixXd& currentDesign, double numbercols, double numberrows);

double calculateDEffNN(const Eigen::MatrixXd& currentDesign, double numbercols);

bool isSingular(const Eigen::MatrixXd& currentDesign);

double calculateCustomOptimality(const Eigen::MatrixXd& currentDesign, Rcpp::Function customOpt);

void rankUpdate(Eigen::MatrixXd& vinv, const Eigen::VectorXd& pointold, const Eigen::VectorXd& pointnew,
                const Eigen::MatrixXd& identity,
                Eigen::MatrixXd& f1, Eigen::MatrixXd& f2,Eigen::MatrixXd& f2vinv);

Eigen::MatrixXd rankUpdateValue(Eigen::MatrixXd& vinv, const Eigen::VectorXd& pointold, const Eigen::VectorXd& pointnew,
                                const Eigen::MatrixXd& identity,
                                Eigen::MatrixXd& f1, Eigen::MatrixXd& f2,Eigen::MatrixXd& f2vinv);

void search_candidate_set(const Eigen::MatrixXd& V, const Eigen::MatrixXd& candidatelist_trans,
                          const Eigen::VectorXd& designrow,
                          double xVx, int& entryy, bool& found, double& del);

//**********************************************************
//Everything below is for generating blocked optimal designs
//**********************************************************

double calculateBlockedDOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& gls);

double calculateBlockedDOptimalityLog(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& gls);

double calculateBlockedIOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& momentsMatrix,const Eigen::MatrixXd& gls);

double calculateBlockedAOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls);

double calculateBlockedAliasTrace(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix,const Eigen::MatrixXd& gls);

double calculateBlockedGOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& gls);

double calculateBlockedTOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls);

double calculateBlockedEOptimality(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls);

double calculateBlockedDEff(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls);

double calculateBlockedDEffNN(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls);

double calculateBlockedAliasTracePseudoInv(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& aliasMatrix,const Eigen::MatrixXd& gls);

bool isSingularBlocked(const Eigen::MatrixXd& currentDesign,const Eigen::MatrixXd& gls);

double calculateBlockedCustomOptimality(const Eigen::MatrixXd& currentDesign, Rcpp::Function customBlockedOpt, const Eigen::MatrixXd& gls);
