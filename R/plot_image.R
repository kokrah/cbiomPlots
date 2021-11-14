#' Plot a factor_data.frame, a character matrix or a numeric matrix
#' 
#' @author Kwame Okrah, \email{kwame.okrah@gmail.com}
#' 
#' @param dat a numeric/character matrix or a data.frame of factors
#' @param col_key a color key corresponding dat (a vector, a list or a function)
#' @param title the plot title
#' @param show_rownames whether or not to show rownames (TRUE/FALSE)
#' @param show_border whether or not to show image border lines (TRUE/FALSE)
#' 
plot_image = function(dat, col_key = NULL, title = NULL, show_rownames = TRUE,
                      show_border = TRUE) {
  res = NULL
  
  # data.frame check
  if ("data.frame" %in% class(dat)) {
    if (is.null(col_key)) {
      stop("Please specify the appropriate col_key for dat (K.Okrah)\n")
    }
    res = cat_df_to_num(dat = dat, col_key_list = col_key)
  }
  
  # matrix check
  if ("matrix" %in% class(dat)) {
    if ("character" == mode(dat)) {
      if (is.null(col_key)) {
        stop("Please specify the appropriate col_key for dat (K.Okrah)\n")
      }
      res = cat_mat_to_num(dat = dat, col_key = col_key)
    }else{
      if ("numeric" == mode(dat)) {
        if (is.null(col_key)) {
          col_key = colorRampPalette(c("blue3", "blue", "white", "red", "red3"))(100)
        }
        res = num_mat_to_num(dat = dat, col_key = col_key)
      }else{
        stop("dat must be a numeric or character matrix (K.Okrah)\n")
      }
    }
  }
  
  if (is.null(res)) {
    stop("dat must be a data.frame or matrix (K.Okrah)\n")
  }
  
  mat = res$res
  col = res$res_col
  
  # make plot
  primary_image(mat, col = col)
  
  # draw border
  if (show_border) {
    draw_border(mat)
  }
  
  # draw rownames
  if (show_rownames) {
    nam = rownames(mat)
    axis(2, nrow(mat):1, nam, las = 2, tick = FALSE, mgp = c(0, 0.2, 0),
         cex.axis = 0.9)
  }
  
  # draw title
  if (is.null(title)) {
    mtext("Panel: Put title here", 1, 0.1, cex = 0.7)
  }else{
    mtext(title, 1, 0.1, cex = 0.7)
  }
  
  nr = nrow(mat)
  nc = nrow(mat)
  
  return(invisible(list(mat = mat,
                        nr = nr,
                        nc = nc,
                        col_key = col_key,
                        title = title,
                        show_border = show_border,
                        show_rownames = show_rownames)))
}