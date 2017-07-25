#'@title Detect and list disallowed combinations in candidate set
#'
#'@description Detects and list disallowed combinations in candidate set
#'
#'@param candidateset Block list
#'@keywords internal
#'@return Returns a list of the disallowed combinations

disallowed_combinations = function(candidateset) {

  inputlevels = lapply(candidateset,unique)
  fullcandidateset = expand.grid(inputlevels)
  anydisallowed = nrow(candidateset) != nrow(fullcandidateset)

  if(anydisallowed) {
    combinedcandidatesets = rbind(fullcandidateset,candidateset)
    return(combinedcandidatesets[!duplicated(combinedcandidatesets,fromLast=TRUE) & !duplicated(combinedcandidatesets),])
  } else {
    return(data.frame())
  }
}
