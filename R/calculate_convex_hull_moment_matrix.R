#' @title Compute convex hull in half-space form
#' @param points A matrix or data frame of numeric coordinates, size n x d
#' @keywords internal
#'
#' @return A list with elements:
#'   - A: a matrix of size m x d (m = number of facets)
#'   - b: a length-m vector
#'   so that x is inside the hull if and only if A %*% x + b >= 0 (for all rows).
#'   - volume: Total volume of the convex hull
convhull_halfspace = function(points) {
  pts_mat = as.matrix(points)

  ch = geometry::convhulln(pts_mat, options = "Fa", output.options = "n")
  chvol = geometry::convhulln(pts_mat, options = "Fa", output.options = "FA")

  hull_facets = ch$hull
  facet_norms = ch$normals

  m = nrow(facet_norms)  # number of facets
  d = ncol(facet_norms)  # dimension

  A = facet_norms[,seq_len(d-1), drop=FALSE]
  b = facet_norms[,d]

  list(
    A       = A,
    b       = b,
    volume  = chvol$vol
  )
}

#' @title Interpolate points within a convex hull
#'
#' @param points A numeric matrix (n x d).
#' @param ch_halfspace Convex hull. Output of `convhull_halfspace`
#' @param n_samples_per_dimension Number of samples per dimension (pre filtering) to take
#'
#' @keywords internal
#'
#' @return A list with:
#'   - data: The interpolated points
#'   - on_edge: A logical vector indicating whether the points
#'
interpolate_convex_hull = function(points, ch_halfspace, n_samples_per_dimension = 11) {
  stopifnot(is.matrix(points), ncol(points) >= 2)

  facet_normals = ch_halfspace$A  # same number of rows as hull_facets
  facet_offsets = ch_halfspace$b  # same number of rows as hull_facets

  ranges = apply(points, 2, range)

  eg_list = list()
  for(i in seq_len(ncol(ranges))) {
    eg_list[[i]] = seq(ranges[1,i], ranges[2,i], length.out=n_samples_per_dimension)
  }
  numeric_eg = do.call(expand.grid,args = eg_list)
  filter_pts_edge = rep(FALSE, nrow(numeric_eg))
  filter_pts = rep(TRUE, nrow(numeric_eg))

  for(i in seq_len(nrow(facet_normals))) {
    vec_single = facet_normals[i,]
    proj_pts = apply(numeric_eg, 1, \(x) sum(x*vec_single) + facet_offsets[i])
    filter_pts = filter_pts & proj_pts < 1e-8
    filter_pts_edge = filter_pts_edge | abs(proj_pts) < 1e-8
  }
  return(list(data=numeric_eg[which(filter_pts),,drop=FALSE],
              on_edge = filter_pts_edge[filter_pts]))
}

#' @title Approximate continuous moment matrix over a numeric bounding box
#'
#' @description Treats the min–max range of each numeric column as a bounding box
#' and samples points uniformly to approximate the integral of the outer product of
#' the model terms, weighted by whether the point
#' is on the edge.
#'
#' @param candidate_set_full Candidate set
#' @param formula Default `~ .`. Model formula specifying the terms.
#' @param n_samples_per_dimension Default `100`. Number of samples to take per dimension when interpolating inside
#' the convex hull.
#'
#' @keywords internal
#' @return A matrix of size `p x p` (where `p` is the number of columns in the model matrix),
#'   approximating the continuous moment matrix.
#'
gen_momentsmatrix_continuous = function(
    formula = ~ .,
    candidate_set,
    n_samples_per_dimension
) {
  # Identify numeric columns (continuous factors)
  is_numeric_col = vapply(candidate_set, is.numeric, logical(1))
  numeric_cols = names(candidate_set)[is_numeric_col]
  factor_cols = names(candidate_set)[!is_numeric_col]

  if (length(numeric_cols) == 0) {
    stop("No numeric columns found in data. Cannot compute continuous bounding box.")
  }
  # Detect any disallowed combinations
  unique_vals = prod(vapply(candidate_set, \(x) {length(unique(x))}, FUN.VALUE = integer(1)))
  any_disallowed = unique_vals != nrow(candidate_set)
  M_acc = NA
  total_weight = 0

  # Simple if all numeric: just integrate over the region.
  if(length(factor_cols) == 0) {
    sub_candidate_set = as.matrix(candidate_set)
    if(ncol(sub_candidate_set) == 1) {
      new_pts_ch = matrix(seq(min(sub_candidate_set),
                       max(sub_candidate_set),
                       length.out = n_samples_per_dimension),ncol=1)
      interp_ch = list()
      interp_ch$on_edge = rep(FALSE, nrow(new_pts_ch))
      vol = max(sub_candidate_set) - min(sub_candidate_set)
      } else {
      ch = convhull_halfspace(sub_candidate_set)
      if (ch$volume <= 0) {
        next
      }
      vol = ch$volume
      interp_ch = interpolate_convex_hull(as.matrix(sub_candidate_set), ch, n_samples_per_dimension = n_samples_per_dimension)
      new_pts_ch = interp_ch$data
    }

    colnames(new_pts_ch) = numeric_cols
    interp_df = as.data.frame(new_pts_ch)

    # Now build model matrix
    Xsub = model.matrix(formula, data = interp_df)

    w = rep(1, nrow(Xsub))
    w[interp_ch$on_edge] = 0.5
    # average subregion moment
    Xsub_w =  apply(Xsub,2,\(x) x*sqrt(w))

    M = crossprod(Xsub_w) / sum(w)

    #Scale by the intercept
    if(colnames(M)[1] == "(Intercept)") {
      M = M / M[1,1]
    }
    return(M)
  } else {
    M_acc = NA
    # For categorical factors with disallowed combinations, we need to account for the
    # reduced domain of the integral. We'll calculate a moment matrix as above for each
    # factor level combination, weigh it by the total number of points, and sum it. That
    # will give us the average prediction variance for the constrained region.
    unique_combos = unique(candidate_set[,factor_cols,drop = FALSE])

    get_contrasts_from_candset = function(candset) {
      csn = colnames(candset)[!unlist(lapply(candset, is.numeric))]
      lapply(csn,
             \(x) {
               setNames(
                 list(skpr::contr.simplex),
                 x[[1]])
             }) |> unlist()
    }

    # We'll accumulate a weighted sum of sub-matrices
    total_weight = 0

    for (r in seq_len(nrow(unique_combos))) {
      combo_row = unique_combos[r, , drop=FALSE]

      # subset of 'candidate_set' that matches this combo
      is_match = TRUE
      for (fc in factor_cols) {
        is_match = is_match & (candidate_set[[fc]] == combo_row[[fc]])
      }
      sub_candidate_set = candidate_set[is_match, , drop=FALSE]
      sub_candidate_set = sub_candidate_set[,is_numeric_col, drop = FALSE]
      # If no rows => disallowed or doesn't appear => skip
      if (nrow(sub_candidate_set) == 0) {
        next
      }
      if(ncol(sub_candidate_set) == 1) {
        new_pts_ch = matrix(seq(min(sub_candidate_set),
                                max(sub_candidate_set),
                                length.out = n_samples_per_dimension),ncol=1)
        interp_ch = list()
        interp_ch$on_edge = rep(FALSE, nrow(new_pts_ch))
        vol = max(sub_candidate_set) - min(sub_candidate_set)
      } else {
        ch = convhull_halfspace(sub_candidate_set)
        if (ch$volume <= 0) {
          next
        }
        vol = ch$volume
        interp_ch = interpolate_convex_hull(as.matrix(sub_candidate_set), ch,
                                            n_samples_per_dimension = n_samples_per_dimension)
        new_pts_ch = interp_ch$data
      }

      colnames(new_pts_ch) = numeric_cols
      interp_df = as.data.frame(new_pts_ch)

      # Pin the factor columns at this combo
      for (fc in factor_cols) {
        interp_df[[fc]] = combo_row[[fc]]
      }

      # Now build model matrix
      Xsub = model.matrix(formula, data = interp_df,
                          contrasts.arg = get_contrasts_from_candset(candidate_set))

      w = rep(1, nrow(Xsub))
      w[interp_ch$on_edge] = 0.5
      # average subregion moment
      Xsub_w =  apply(Xsub,2,\(x) x*sqrt(w))
      # M_sub = crossprod(Xsub) / sum(w)

      M_sub = crossprod(Xsub_w) / sum(w)

      # Weighted accumulation
      if (all(is.na(M_acc))) {
        M_acc = vol * M_sub
      } else {
        M_acc = M_acc + vol * M_sub
      }
      total_weight = total_weight + vol
    }

    M = M_acc / total_weight
    #Scale by the intercept
    if(colnames(M)[1] == "(Intercept)") {
      M = M / M[1,1]
    }
    return(M)
  }
}
