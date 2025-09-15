#'@title Gets or calculates the moment matrix, given a candidate set, a design, and a model
#'
#'@description Returns number of levels prior to each parameter
#'
#'@keywords internal
#'@return Returns a vector consisting of the number
#'of levels preceeding each parameter (including the intercept)
get_moment_matrix = function(
  design = NA,
  factors = NA,
  classvector = NA,
  candidate_set_normalized = NA,
  model = ~.,
  moment_sample_density = 10,
  high_resolution_candidate_set = NA
) {
  if (all(is.na(design))) {
    if (
      all(is.na(candidate_set_normalized)) &&
        all(is.na(high_resolution_candidate_set))
    ) {
      stop("Bug: This branch should never be reached.")
    }
    # We are calculating the moment matrix for the first time
    any_disallowed = detect_disallowed_combinations(candidate_set_normalized)
    stopifnot(!all(is.na(factors)) && !all(is.na(classvector)))

    if (all(classvector) || !any_disallowed) {
      mm = gen_momentsmatrix_ideal(factors, classvector)
    } else {
      if (all(is.na(high_resolution_candidate_set))) {
        #If any disallowed and there are any numeric cols, use convex hull
        if (any_disallowed) {
          warning(
            "Generating a convex hull of the candidate set to generate a ",
            "dense sampling of the space for a numerical integration of I-optimality. ",
            "For a faster and more accurate calculation, generate the high resolution ",
            "candidate set yourself and pass it to `high_resolution_candidate_set`."
          )
          if (!all(classvector)) {
            #Hash the inputs so we only have to calculate the moment matrix once for a given input
            hash_mm = digest::digest(list(
              attr(terms.formula(model), "factors"), #Use the factor matrix to avoid spurious changes related to capture environments
              candidate_set_normalized,
              moment_sample_density
            ))
            if (!exists(hash_mm, envir = skpr_moment_matrix_cache)) {
              mm = gen_momentsmatrix_constrained(
                formula = model,
                candidate_set = candidate_set_normalized,
                n_samples_per_dimension = moment_sample_density
              )
              assign(
                hash_mm,
                mm,
                envir = skpr_moment_matrix_cache
              )
            } else {
              mm = get(hash_mm, envir = skpr_moment_matrix_cache)
            }
          } else {
            mm = gen_momentsmatrix_ideal(
              factors,
              classvector
            )
          }
        } else {
          stop("Bug: This branch should never be reached.")
        }
      } else {
        high_resolution_candidate_set_norm = normalize_design(
          high_resolution_candidate_set
        )
        mm = gen_momentsmatrix_constrained(
          formula = model,
          candidate_set = high_resolution_candidate_set_norm,
          n_samples_per_dimension = moment_sample_density,
          user_provided_high_res_candidateset = TRUE
        )
      }
    }
  } else {
    # Otherwise, we check for an existing one and only calculate it if need be
    imported_mm = FALSE
    if (!is.null(attr(design, "generating_model"))) {
      og_design_factors = attr(
        terms.formula(attr(design, "generating_model")),
        "factors"
      )
      new_model_factors = attr(terms.formula(model), "factors")
      if (all(dim(og_design_factors) == dim(new_model_factors))) {
        identical_main_effects = all(
          rownames(og_design_factors) == rownames(new_model_factors)
        )
        identical_interactions = all(
          colnames(og_design_factors) == colnames(new_model_factors)
        )
        if (identical_interactions && identical_main_effects) {
          if (
            all(!is.na(attr(design, "moments.matrix"))) &&
              all(!is.null(attr(design, "moments.matrix")))
          ) {
            mm = attr(design, "moments.matrix")
            imported_mm = TRUE
          }
        }
      }
    }
    if (!imported_mm) {
      #We did not detect an existing moment matrix
      has_cs_attr = !is.null(attr(design, "candidate_set"))
      if (all(is.na(high_resolution_candidate_set))) {
        #No user provided high res candidate set
        if (!has_cs_attr) {
          warning(
            "If a design was not generated in `skpr` (and thus does not have the 'candidate_set' attribute), ",
            "you must pass a candidate set to `high_resolution_candidate_set` to calculate I-optimality."
          )
          mm = NA
        } else {
          #Calculate a dense sampling based off the convex hull: This is slow
          candidate_set_normalized = normalize_design(attr(
            design,
            "candidate_set"
          ))
          any_disallowed = detect_disallowed_combinations(
            candidate_set_normalized
          )

          if (any_disallowed) {
            warning(
              "Generating a convex hull of the original candidate set to generate a ",
              "dense sampling of the space for a numerical integration of I-optimality. ",
              "For a faster and more accurate calculation, generate the high resolution ",
              "candidate set yourself and pass it to `high_resolution_candidate_set`."
            )
            if (!all(classvector)) {
              #Hash the inputs so we only have to calculate the moment matrix once for a given input
              hash_mm = digest::digest(list(
                attr(terms.formula(model), "factors"), #Use the factor matrix to avoid spurious changes related to capture environments
                candidate_set_normalized,
                moment_sample_density
              ))
              if (!exists(hash_mm, envir = skpr_moment_matrix_cache)) {
                mm = gen_momentsmatrix_constrained(
                  formula = model,
                  candidate_set = candidate_set_normalized,
                  n_samples_per_dimension = moment_sample_density
                )
                assign(
                  hash_mm,
                  mm,
                  envir = skpr_moment_matrix_cache
                )
              } else {
                mm = get(hash_mm, envir = skpr_moment_matrix_cache)
              }
            } else {
              mm = gen_momentsmatrix_ideal(
                factors,
                classvector
              )
            }
          } else {
            # Use closed form solution
            mm = gen_momentsmatrix_ideal(
              factors,
              classvector
            )
          }
        }
      } else {
        high_resolution_candidate_set_norm = normalize_design(
          high_resolution_candidate_set
        )
        mm = gen_momentsmatrix_constrained(
          formula = model,
          candidate_set = high_resolution_candidate_set_norm,
          n_samples_per_dimension = moment_sample_density,
          user_provided_high_res_candidateset = TRUE
        )
      }
    } #end if(imported_mm)
  }
  return(mm)
}
