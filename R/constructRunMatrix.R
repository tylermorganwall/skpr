#'@title Construct Run Matrix given rows
#'
#'@description Returns number of levels prior to each parameter
#'
#'@keywords internal
#'@return Returns a vector consisting of the number
#'of levels preceeding each parameter (including the intercept)
constructRunMatrix = function(rowIndices, candidateList, augment = NULL) {
  trials = length(rowIndices)
  run_matrix = as.data.frame(candidateList[1:trials, ])
  colnames(run_matrix) = colnames(candidateList)
  for (i in 1:trials) {
    run_matrix[i, ] = candidateList[rowIndices[i], ]
  }
  if (!is.null(augment)) {
    run_matrix[1:nrow(augment), ] = augment
  }
  return(run_matrix)
}
