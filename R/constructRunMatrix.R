#'@title Construct Run Matrix given rows
#'
#'@description Returns number of levels prior to each parameter
#'
#'@param Levelvector Returns a vector consisting of the number
#'@keywords internal
#'@return Returns a vector consisting of the number
#'of levels preceeding each parameter (including the intercept)
constructRunMatrix = function(rowIndices, candidateList) {
  trials = length(rowIndices)
  runMatrix = as.data.frame(candidateList[1:trials,])
  colnames(runMatrix) = colnames(candidateList)
  for(i in 1:trials) {
    runMatrix[i,] = candidateList[rowIndices[i],]
  }
  return(runMatrix)
}
