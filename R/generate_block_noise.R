
#' Title Generate total noise from blocks in each run
#'
#' @param blockindicators Run Matrix indicating which block each run is in. The last column is assumed
#' to be a run number, and is therefore ignored. i.e. the number of columns must be one greater than the
#' number of blocking layers.
#' @param levels_in_block List of the levels in each block. The i-th entry in the list must be a vector
#' of the levels of the i-th block layer.
#' @param blocknoise Vector of standard deviation of noise in each block, one entry per block layer.
#'
#' @return Returns a vector of the total noise in each run, simulated with rnorm.
#' @export
#'
#' @examples
#' library(plyr)
#'
#' blockmatrix = cbind(c(1,1,1,1,2,2,2,2), c('a', 'a', 'b', 'b', 'c', 'c', 'd', 'd'), 1:8)
#' levs = alply(blockmatrix, 2, unique)
#' noiselevels = c(10, 1)
#' generate_block_noise(blockmatrix, levs, noiselevels)
#'
generate_block_noise <- function(blockindicators, levels_in_block, blocknoise) {
  #Each block gets a single random draw, and that draw is replicated for each run in the block
  noise_from_blocks = rep(0, nrow(blockindicators)) #this will hold the total random noise due to blocks in each run
  if(!is.null(blocknoise)) {
    #the last column of blockindicators is the run number, not a block indicator
    for(i in 1:(ncol(blockindicators) - 1)) {
      blocknoise_realized = rnorm(length(levels_in_block[[i]]), mean = 0, sd = blocknoise[i])
      names(blocknoise_realized) = levels_in_block[[i]]
      noise_from_blocks = noise_from_blocks + blocknoise_realized[blockindicators[, i]]
    }
  }
  noise_from_blocks
}
