#' Convert a character matrix to a numerical matrix
#' 
#' This is an internal function.
#'  
#' @author Kwame Okrah, \email{kwame.okrah@gmail.com}
#'  
#' @param dat a character matrix
#' @param col_key color key for the levels of dat (a named char vector)
#' 
cat_mat_to_num = function(dat, col_key) {
  if (!"matrix" %in% class(dat)) {
    stop("Stop dat must be a matrix (K.Okrah)\n")
  }else{
    if (!"character" == mode(dat)) {
      stop("Stop dat must be a character matrix (K.Okrah)\n")
    }
  }
  
  ux = sort(unique(dat[!is.na(dat)]))
  col_levels = names(col_key)
  check_x_levels = ux %in% col_levels
  
  if (!all(check_x_levels)) {
    problem_levels = paste0(ux[!check_x_levels], collapse = ", ")
    msg = paste0("The ff dat level(s): '", problem_levels,
                 "' is/are not in the col_key levels (K.Okrah)\n")
    stop(msg)
  }
  
  nr = nrow(dat)
  nc = ncol(dat)
  res = matrix(NA, nrow = nc, ncol = nr)
  colnames(res) = rownames(dat)
  rownames(res) = colnames(dat)
  
  col_levels = col_levels[col_levels %in% ux]
  
  for (i in 1:nc) {
    res[i,] = as.numeric(factor(dat[,i], col_levels)) 
  }
  
  res_col = col_key[col_levels]
  
  # make legend
  # make legend key
  legend_shift =  (max(res) - min(res)) / length(res_col)
  legend_breaks = seq(min(res), max(res) - legend_shift, legend_shift)
  legend_breaks = legend_breaks + legend_shift / 2
  legend_labels = names(res_col)
  
  list(res = res, res_col = res_col, legend_breaks = legend_breaks, 
       legend_labels=legend_labels)
}