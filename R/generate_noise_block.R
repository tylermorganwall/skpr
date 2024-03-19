#'@title Generate Noise Block
#'
#'@description Generates the noise to be added in the REML power calculation
#'
#'@return Noise vector
#'@keywords internal
generate_noise_block = function(noise, groups, blockstructure) {
  total_noise_matrix = matrix(0,nrow=sum(groups[[length(groups)]]), ncol = length(groups)-1)
  for (i in seq_len(length(groups) - 1)) { #Not including the run-to-run noise
    name_blocks = names(groups[[i]])
    blocktemp = list()
    for (j in seq_len(length(groups[[i]]))) {
      noise_idx = which(name_blocks[j] == blockstructure[,i])
      block_noise = rnorm(n = 1, mean = 0, sd = sqrt(noise[i]))
      total_noise_matrix[noise_idx, i] = block_noise
    }
  }
  totalblocknoise = apply(total_noise_matrix, 1, sum)
  return(totalblocknoise)
}
