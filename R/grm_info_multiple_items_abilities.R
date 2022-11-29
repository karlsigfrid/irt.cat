#' Calculate the grm information
#'
#' In this version we apply the function to multiple ability levels at once.
#'
#' Calculate the information of one or multiple response items.
#' The unidimensional graded response model is assumed.
#' Note that the 2PL dichotomous model is a special case of the graded response model, with only two levels.
#'
#' @param theta The ability or abilities for which to estimate the information.
#' @param item_params matrix of the item parameters. The first column contains the discrimination parameters and the subsequent columns contain the difficulty parameters.
#' @param constant_D If true, include the constant D=1.702 in the formula for calculating the information. (Reckase 2009, page 52)
#' @return A vector of length N, where N is the number of abilities included in theta.
#' @examples
#' my_item_params <- matrix(c(1, 1.5, -1, -0.5, 0, 1), ncol=3)
#' grm_info_multiple_items_abilities(theta=c(-0.5, 0, 0.5, 1), item_params=my_item_params)
#'



grm_info_multiple_items_abilities <- function(theta, item_params, constant_D=F){
  the_info <- apply(item_params, 1, function(q){
    grm_info_one_item(theta=theta, item_params = q, constant_D = constant_D)
  })
  colnames(the_info) <- paste("item", 1:nrow(item_params))
  rownames(the_info) <- paste("ability", 1:length(theta))
  the_info
}


grm_info_one_item <- function(theta, item_params, constant_D=F){
  D <- ifelse(constant_D, 1.702, 1)
  a <- item_params[1]
  d <- matrix(item_params[-1], nrow=1)
  n <- length(theta)
  z <- a * (theta - d[rep(1, n), ])
  P <- cbind(1, plogis(q=z), 0)
  P1 <- P[, -ncol(P)]
  P2 <- P[, -1]
  I_m <- ((D * a * P1 * (1 - P1) -
             D * a * P2 * (1 - P2))^2) / (P1 - P2)
  rowSums(I_m)
}
