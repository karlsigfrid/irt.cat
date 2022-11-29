#' Calculate the bandwidth according to Silverman's rule of thumb.
#'
#'Silvermans rule of thumb uses the formula
#'bw = 0.9 * min(std.dev, IQR/1.34) * n^(-1/5)
#'
#'
#' @param m A vector or a matrix. If a matrix is used as argument, Silverman's rule will be applied to each column separately.
#' @return A vector of bandwidth values. If m is a vector, the function will return a single value. If m is a matrix of P columns, a vector of length P will be returned.
#' @examples
#' set.seed(123)
#' m <- matrix(c(3,3,10,2,6,5,4,6,9,10), ncol=2)
#' bw_silverman(m=m)
#' [1] 1.460377 1.688441

bw_silverman <- function(m){
  m <- as.matrix(m)
  n <- nrow(m)
  std_dev <- apply(m, 2, sd)
  the_iqr <- apply(m, 2, IQR) / 1.34
  A <- pmin(std_dev, the_iqr)
  0.9 * A * n^(-1/5)
}

#This function may be superfluous as it does the same thing as bw.ndr0
