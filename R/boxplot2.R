#' Boxplot with points
#'  
#' @author Kwame Okrah, \email{kwame.okrah@gmail.com}
#'  
#' @param y a numeric vector
#' @param x a character vector
#' @param pt.col points color 
#' @param ... pass arguments to the \code{boxplot} function
boxplot2 = function(y, x, pt.col = NULL, ...) {
  boxplot(y ~ x, col = NA, ...)
  xp = jitter(as.numeric(factor(x)), factor = 0.9)
  if (is.null(pt.col)) {
    col = densCols(xp, y)
  }
  points(xp, y, pch = 19, col = pt.col)
}