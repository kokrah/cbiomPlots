#' Prepares a numerical matrix for plotting
#' 
#' This is an internal function.
#'  
#' @author Kwame Okrah, \email{kwame.okrah@gmail.com}
#'  
#' @param dat a numeric matrix
#' @param col_key a color for dat
num_mat_to_num = function(dat, col_key) {
  if (!"matrix" %in% class(dat)) {
    stop("Stop dat must be a matrix (K.Okrah)\n")
  }else{
    if (!"numeric" == mode(dat)) {
      stop("Stop dat must be a numeric matrix (K.Okrah)\n")
    }
  }
  
  res = t(dat)
  res_col = col_key
  
  list(res = res, res_col = res_col)
}