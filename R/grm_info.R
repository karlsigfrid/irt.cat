#' Calculate the grm information
#'
#' Calculate the information of one or multiple response items.
#' The unidimensional graded response model is assumed.
#' Note that the 2PL dichotomous model is a special case of the graded response model, with only two levels.
#'
#' @param theta The ability for which to estimate the information.
#' @param item_params A matrix of the item parameters. Each row represents one question item. The first column contains the discrimination parameter and the subsequent columns contain the difficulty parameters.
#' @param constant_D If true, include the constant D=1.702 in the formula for calculating the information. (Reckase 2009, page 52)
#' @return A vector of length N, where N is the number of items included in item_params.
#' @examples
#' my_item_params <- matrix(c(1, 1.5, -1, -0.5, 0, 1), ncol=3)
#' grm_info(theta=0, item_params=my_item_params)
#'


grm_info <- function(theta, item_params, constant_D=F){
  #D=1.702 makes the calculation consistent with Reckase (2009)
  D <- ifelse(constant_D, 1.702, 1)
  a <- item_params[, 1]
  d <- item_params[, 2:ncol(item_params)]
  z <- a * (theta - d)
  P <- cbind(1, plogis(q=z), 0)
  P1 <- P[, -ncol(P)]
  P2 <- P[, -1]
  I_m <- ((D * a * P1 * (1 - P1) -
             D * a * P2 * (1 - P2))^2) / (P1 - P2)
  rowSums(I_m)
}
