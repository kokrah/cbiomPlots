#' Draw border
#'
#' This is an internal function.
#' 
#' @author Kwame Okrah, \email{kwame.okrah@gmail.com}
#' 
#' @param x a matrix 
draw_border = function(x, cb = TRUE, rb = TRUE) {
  nr = nrow(x)
  nc = ncol(x)
  if (cb) abline(v = (2:nc) - 0.5, col = "white")
  if (rb) abline(h = (2:nr) - 0.5, col = "white")
}