#' Convert a character data.frame to a numerical matrix
#' 
#' This is an internal function.
#'  
#' @author Kwame Okrah, \email{kwame.okrah@gmail.com}
#'  
#' @param dat a character data.frame
#' @param col_key_list color key list for the levels of each column
#' in dat (a list of named char vectors)
cat_df_to_num = function(dat, col_key_list) {
  if (!"data.frame" %in% class(dat)) {
    stop("Stop dat must be a data.frame (K.Okrah)\n")
  }

  if (!"list" %in% class(col_key_list)) {
    stop("Stop col_key_list must be a list (K.Okrah)\n")
  }
  
  dat_names = names(dat)
  col_key_list_names = names(col_key_list)
  
  if (length(dat_names) != length(col_key_list)) {
    msg = paste0("Stop dat colnames and col_key_list names", 
                 " must be the same (K.Okrah)\n")
    stop(msg)
  }

  if (!all(sort(dat_names) == sort(col_key_list_names))) {
    msg = paste0("Stop dat colnames and col_key_list names", 
                 " must be the same (K.Okrah)\n")
    stop(msg)
  }

  nr = nrow(dat)
  nc = ncol(dat)
  res = matrix(NA, nrow = nc, ncol = nr)
  colnames(res) = rownames(dat)
  rownames(res) = colnames(dat)
  res_col_levels = list()
  
  for (i in names(col_key_list)) {
    x = as.character(dat[,i])
    col_levels = names(col_key_list[[i]])
    ux = sort(unique(x[!is.na(x)]))
    check_x_levels = ux %in% col_levels
  
    if (!all(check_x_levels)) {
      problem_levels = paste0(ux[!check_x_levels], collapse = ", ")
      msg = paste0("In column ", i, " level(s): '", problem_levels,
                   "' is/are not in the corresponding col_key_list",
                   " levels (K.Okrah)\n")
      stop(msg)
    }

    col_levels = col_levels[col_levels %in% ux]
    res_col_levels[[i]] = col_levels
    res[i,] = as.numeric(factor(x, col_levels))
  }
  
  col_shift = sapply(res_col_levels, length)
  col_shift = cumsum(col_shift)
  res = sweep(res, 1, col_shift, "+") - col_shift[1]
  
  res_col = c()
  for (i in names(col_key_list)) {
    res_col = c(res_col, col_key_list[[i]][res_col_levels[[i]]])
  }
  names(res_col) = NULL
  
  list(res = res, res_col = res_col)
}