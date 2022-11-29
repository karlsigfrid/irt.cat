#' Simulate response data
#'
#' Simulate response data, given a vector of theta values and a matrix of item parameters.
#'
#' @param theta A vector of theta values, representing the abilities of the simulated test takers.
#' @param item_params A matrix of the item parameters. Each row represents one question item. The first column contains the discrimination parameter and the subsequent columns contain the difficulty parameters.
#' @return An A x B dimensioned data frame, where A is the number of simulated test takers, and B the number of question items. The values of the responses are in the interval [1, #levels], where #levels is determined by the argument item_params.
#' @examples
#' my_abilities <- rnorm(n=100)
#' my_item_params <- matrix(c(1, 1, 1, -1, 0, 1), ncol=2)
#' simulate_response_grm(theta=my_abilities, item_params=my_item_params)

simulate_response_grm <- function(theta, item_params){
  no_q <- nrow(item_params)
  no_l <- ncol(item_params)
  p_array <- make_p_array_grm(theta=theta, item_params=item_params)
  sim_data <- c()
  for (i in 1:no_q){
    this_q <- apply(p_array[, , i], 1, function(q){
      sample(x=1:no_l, size=1, replace=T, prob=q)
    })
    sim_data <- cbind(sim_data, this_q)
  }
  data.frame(sim_data)
}

make_p_array_grm <- function(theta, item_params){
  item_params <- as.matrix(item_params)
  no_u <- length(theta)
  no_q <- nrow(item_params)
  no_l <- ncol(item_params)
  a <- item_params[, 1]
  d <- item_params[, -1, drop=F]

  z_array <- array(data=NA, dim=c(no_u, no_l - 1 , no_q))
  for (i in 1:no_q) z_array[, , i] <-
    a[i] * (theta - d[rep(i, no_u), , drop=F])

  p_cum_array <- array(data=NA, dim=c(no_u, no_l + 1, no_q))
  for (i in 1:no_q) p_cum_array[, , i] <-
    cbind(1, plogis(q=z_array[, , i]), 0)

  p_array <- array(data=NA, dim=c(no_u, no_l , no_q))
  for (i in 1:no_q) p_array[, , i] <-
    p_cum_array[, -(no_l+1), i] - p_cum_array[, -1, i]
  p_array
}
