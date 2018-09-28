#'@title Generate Noise Block
#'
#'@description Generates the noise to be added in the REML power calculation
#'@param RunMatrix The run matrix
#'@return Normalized run matrix
#'@keywords internal
generate_noise_block = function(noise, groups) {
  listblocknoise = list()
  for (i in 1:(length(groups) - 1)) {
    blocktemp = list()
    for (j in 1:length(groups[[i]])) {
      row = rnorm(n = 1, mean = 0, sd = sqrt(noise[i]))
      blocktemp[[j]] = do.call(rbind, replicate(groups[[i]][j], row, simplify = FALSE))
    }
    listblocknoise[[i]] = do.call(rbind, blocktemp)
  }
  totalblocknoise = Reduce("+", listblocknoise)
  return(totalblocknoise)
}
