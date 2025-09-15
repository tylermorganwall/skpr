design_df_to_processed_list = function(
  design,
  model,
  blocking,
  varianceratios,
  nointercept
) {
  design_df = as.data.frame(design)
  design_processed_list = convert_blockcolumn_rownames(
    design_df,
    blocking,
    varianceratios
  )

  new_varianceratios = design_processed_list$varianceratios
  zlist = design_processed_list$z_matrix_list
  design_blockcolumn_converted = design_processed_list$design_blockcolumn_converted

  design_skpr_blockcolumn_removed = remove_skpr_blockcols(
    design_blockcolumn_converted
  )

  #----- Convert dots in formula to terms -----#
  model_converted_dots = convert_model_dots(
    design_skpr_blockcolumn_removed,
    model
  )

  #----- Rearrange formula terms by order -----#
  model_rearranged_terms = rearrange_formula_by_order(
    model_converted_dots,
    data = design_skpr_blockcolumn_removed
  )
  if (nointercept) {
    model_rearranged_terms = update.formula(model_rearranged_terms, ~ -1 + .)
  }

  #---- Reduce run matrix to terms in model ---#
  design_reduced_to_model = reduceRunMatrix(
    design_skpr_blockcolumn_removed,
    model_rearranged_terms
  )

  #------Normalize/Center numeric columns ------#
  design_fully_processed = normalize_design(design_reduced_to_model)

  return(list(
    design_processed = design_fully_processed,
    model_processed = model_rearranged_terms,
    varianceratios = new_varianceratios,
    zlist = zlist
  ))
}
