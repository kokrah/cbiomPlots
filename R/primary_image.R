#' Primary image
#' 
#' This is an internal function.
#'  
#' @author Kwame Okrah, \email{kwame.okrah@gmail.com}
#'  
#' @param x a numeric matrix
#' @param ... pass arguments to the \code{image} function
primary_image = function(x, ...) {
  if (nrow(x) > 1) {
    y = x[nrow(x):1, ]
    y = t(y)  
  }else{
    y = t(x)
  }
  image(1:nrow(y), 1:ncol(y), y, axes = FALSE, xlab = "", ylab = "", ...)
}