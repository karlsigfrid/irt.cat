#' Find the next best variable
#'
#' Given a model , the full data set used to train the model and a vector with the column positions of the already included variables, find the next best variable.
#'
#' @param model The mirt model.
#' @param model_data A matrix or data frame with only one row. The column names should be the same as for the data training the model.
#' @param included_variables a vector with the positions of the columns of the included variables. If NULL (default), the current ability is estimated to 0.
#' @return The function returns the name of the next best variable, the position of the column with the next best variable, the total information of the test with the next variable included and the information of each variable.
#' @examples
#' ref_locations <- rbind(c(0.07, 0.1, -1.1, -1, -0.9),
#' c(-1.5, -0.5, 1.6, 1.5, 1.55))
#' est_theta <- c(-1, -1)
#' min_info <- 4
#' calculate_density(est_theta, ref_locations, min_info)


calculate_density <- function(est_theta, ref_locations, min_info){
  n_dim <- length(est_theta)
  std_min_info <- (min_info - 4) / (1/2) #mean=4, sd=1/2
  bw_divider <- pnorm(std_min_info, mean=0, sd=1)
  # It appears that the value of bw.nrd0 is unaffected by weights.
  bw_undivided <- apply(ref_locations, 1, bw.nrd0)
  my_bw <- max(bw_undivided) / bw_divider
  theta_ref_dist <- sqrt(colSums((ref_locations - est_theta)^2))
  my_weights <- exp(-theta_ref_dist) / sum(exp(-theta_ref_dist))
  my_densities <- apply(ref_locations, 1,
                        density, bw=my_bw, weight=my_weights)
  density_points <- sapply(1:n_dim, function(q){
    this_density <- my_densities[[q]]
    closest_grid_point <- which.min((this_density$x - est_theta[q])^2)
    this_density$y[closest_grid_point]})
  density_points
}
