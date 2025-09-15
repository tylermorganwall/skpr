generate_contrast_list = function(
  design_processed,
  presetcontrasts,
  contrasts
) {
  contrastslist = list()
  contrastslist_cormat = list()
  for (x in names(design_processed[
    lapply(design_processed, class) %in% c("character", "factor")
  ])) {
    if (!(x %in% names(presetcontrasts))) {
      contrastslist[[x]] = contrasts
    } else {
      contrastslist[[x]] = presetcontrasts[[x]]
    }
    contrastslist_cormat[[x]] = contr.simplex
  }
  if (length(contrastslist) < 1) {
    contrastslist = NULL
    contrastslist_cormat = NULL
  }
  return(list(
    contrastslist = contrastslist,
    contrastslist_cormat = contrastslist_cormat
  ))
}
