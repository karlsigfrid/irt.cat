#' Calculate the dimension importance score for each dimension
#'
#' Given a model , the full data set used to train the model and a vector with the column positions of the already included variables, find the next best variable.
#'
#' @param theta A vector with values of theta of length p
#' @param party_pos A data frame. Column 1 is the party name. I addition, one column should be included per dimension.
#' @return A vector of dimension importance scores of length p
#' @examples
#' party_pos <- data.frame(party=c("p1", "p2", "p3"), dim1=c(-2.5, 0, 0.1), dim2=c(0, -0.2, -1.7))
#' theta <- c(-1, -1)
#' calculate_dim_importance(theta=theta, party_pos=party_pos)

calculate_dim_importance <- function(theta, party_pos){
  points <- make_table_points(theta=theta, party_pos=party_pos)
  point_pairs <- make_table_point_pairs(theta, points)
  dim_weight_unnormalized <-
    colSums(point_pairs[, c("net_diff_dim1", "net_diff_dim2")] * point_pairs$weight)
  dim_weight <- dim_weight_unnormalized / sum(dim_weight_unnormalized)
  names(dim_weight) <- paste0("score_dim", 1:length(theta))
  list(dim_weight=dim_weight, points=points, point_pairs=point_pairs)
}

#' Create a table with information about the party points in each dimension
#'
#' Currently, the function is written for 2 dimensions only.
#'
#' @param theta A vector with values of theta
#' @param party_pos A data frame. Column 1 is the party name. I addition, one column should be included per dimension.
#' @return a data frame with columns for (1) party names (2) party positions in all dimensions (3) party distance to theta for each dimension (4) The euclidean distrances from each party to theta (5) for each dimension the change of the euclidean distance to theta as a function of a change in the dimension at the current location of theta.
#' @examples
#' theta <- c(-1, 1)
#' party_pos <- data.frame(party=c("p1", "p2", "p3"), dim1=c(-2.5, 0, 0.1), dim2=c(0, -0.2, -1.7))
#' make_table_points(theta=theta, party_pos=party_pos)

make_table_points <- function(theta, party_pos){
  points <- party_pos
  colnames(points) <- c("party", "dim1", "dim2")
  points$dim1_dist <- theta[1] - points$dim1
  points$dim2_dist <- theta[2] - points$dim2
  points$euclid_dist <- sqrt(points$dim1_dist^2 + points$dim2_dist^2)
  points$deriv_wrt_theta1 <- points$dim1_dist / points$euclid_dist #The derivative of the distance to a point wrt x
  points$deriv_wrt_theta2 <- points$dim2_dist / points$euclid_dist #The derivative of the distance to a point wrt y
#  rownames(points) <- paste("Party", 1:3)
  points
}


#' Create a table with information about all pairs of party points
#'
#' Currently, the function is written for 2 dimensions only.
#'
#' @param theta A vector with values of theta
#' @param points A data frame which is the output of function make_table_points.
#' @return a data frame with information for calculating dimension scores.
#' @examples
#' theta <- c(-1, 1)
#' party_pos <- data.frame(party=c("p1", "p2", "p3"), dim1=c(-2.5, 0, 0.1), dim2=c(0, -0.2, -1.7))
#' points <- make_table_points(theta=theta, party_pos=party_pos)
#' make_table_point_pairs(theta=theta, points=points)

make_table_point_pairs <- function(theta, points){
  point_pairs_x <- expand.grid(points$deriv_wrt_theta1, points$deriv_wrt_theta1)
  point_pairs_y <- expand.grid(points$deriv_wrt_theta2, points$deriv_wrt_theta2)
  point_pairs <- expand.grid(points$party, points$party, stringsAsFactors = F)
  point_pairs <- cbind(point_pairs, point_pairs_x, point_pairs_y)
  colnames(point_pairs) <- c("P1", "P2", "dist1_dim1", "dist2_dim1", "dist1_dim2", "dist2_dim2")
  point_pairs <- point_pairs[point_pairs$P1 < point_pairs$P2, ]
  point_pairs$net_diff_dim1 <- abs(point_pairs$dist1_dim1 - point_pairs$dist2_dim1)
  point_pairs$net_diff_dim2 <- abs(point_pairs$dist1_dim2 - point_pairs$dist2_dim2)
  point_pairs$ssq_euclid <-
    points$euclid_dist[match(point_pairs$P1, points$party)]^2 +
    points$euclid_dist[match(point_pairs$P2, points$party)]^2
  point_pairs$weight <- 1/point_pairs$ssq_euclid
  point_pairs_return <-
    point_pairs[, c("P1", "P2", "net_diff_dim1", "net_diff_dim2", "ssq_euclid", "weight")]
  point_pairs_return
}


#Weigh the importance scores

weigh_importance_scores <- function(importance_scores, min_info, mu=4){
  no_importance_scores <- rep(1 / length(importance_scores),
                              length(importance_scores))
  importance_weight <- pnorm(q=min_info, mean=mu, sd=1)
  weighted_importance_scores <-
    no_importance_scores + (importance_scores - no_importance_scores) *
    importance_weight
  weighted_importance_scores
}
