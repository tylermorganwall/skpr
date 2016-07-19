#'@title Find block sizes in column
#'
#'@description Returns blocks
#'
#'@param blocklist Block list
#'@keywords internal
#'@return Returns the blocking structure given a vector of block ids
blockingstructure = function(blocklist) {
  blocklength = c()
  lengthblock = 1
  for(i in 1:length(blocklist)) {
    counter = 0
    if(lengthblock == 1) {
      for(j in i:length(blocklist)) {
        if(j == length(blocklist) && blocklist[i] == blocklist[j]) {
          counter = counter + 1
          blocklength = c(blocklength, counter)
          return(blocklength)
        }

        if(blocklist[i] == blocklist[j]) {
          counter = counter + 1
        } else {
          blocklength = c(blocklength, counter)
          lengthblock = counter
          break
        }
      }
    } else {
      lengthblock = lengthblock - 1
    }
  }
}
