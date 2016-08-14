
#' Title Generate total noise from blocks in each run
#'
#' @param blockindicators Run Matrix indicating which block each run is in. Must be numeric.
#' @param levels_in_block List of the levels in each block. Each list must be numeric, and the levels
#' must run from 1 .. nblocks
#' @param blocknoise Vector of standard deviation of noise in each level, one entry per block
#'
#' @return Returns a vector of the total noise in each run, simulated with rnorm.
#' @export
#'
#' @examples
#' total_block_noise = generate_block_noise(blockindicators, levels_in_block, blocknoise)
#'
generate_block_noise <- function(blockindicators, levels_in_block, blocknoise) {
  #Each block gets a single random draw, and that draw is replicated for each run in the block
  noise_from_blocks = rep(0, nrow(blockindicators)) #this will hold the total random noise due to blocks in each run
  if(!is.null(blocknoise)) {
    #the last column of blockindicators is the run number, not a block indicator
    for(i in 1:(ncol(blockindicators) - 1)) {
      blocknoise_realized = rnorm(length(levels_in_block[[i]]), mean = 0, sd = blocknoise[i])
      noise_from_blocks = noise_from_blocks + blocknoise_realized[blockindicators[, i]]
    }
  }
  noise_from_blocks
}
