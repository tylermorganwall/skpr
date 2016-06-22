#'@title Calculate block sizes
#'
#'@description Calculate block size vector
#'
#'@param trials Number of trials in design
#'@param blocksize The desired size of each block
#'@return The blocksize vector
#'@keywords internal
calcblocksizes = function(trials,blocksize) {
  if(length(blocksize) > 1) {
    return(blocksize)
  }
  blocks = floor(trials/blocksize)
  extra = trials %% blocksize
  if(extra == 0) {
    extra = NULL
  }
  return(c(rep(blocksize,blocks),extra))
}
