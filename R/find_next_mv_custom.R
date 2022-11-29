#' Find the next best variable, choosing from variables in multiple dimensions.
#'
#' Given N models, the full data set used to train each model and N vectors with the column positions of the already included variables, find the next best variable for each dimension.
#'
#' @param theta A vector of ability parameters in p dimensions.
#' @param item_params A list of item-parameter matrices. The list has length p.
#' @param included_variables A list of p vectors, one vector per dimension.
#' @param party_pos A data frame. Column 1 is the party name. I addition, one column should be included per dimension.
#' @return The function returns the dimension with the next best variable, name of the next best variable, the position of the column with the next best variable, the total information of the test in each dimension with the next variable included. The information in One dimension will be updated, while the information in the other dimensions will remain unchanged.
#' @examples
#'
#' theta <- c(0,0)
#' Science_model <- mirt(data = mirt::Science, model = 1, itemtype = "graded")
#' SAT12_model <- mirt(data = mirt::SAT12[, 1:5], model = 1, itemtype = "graded")
#' items1 <- coef(Science_model, IRTpar=T, simplify=T)$items
#' items2 <- coef(SAT12_model, IRTpar=T, simplify=T)$items
#' item_params <- list(items1, items2)
#' included_variables <- list(c(1, 3), NULL)
#' party_pos <- data.frame(party=c("p1", "p2", "p3"), dim1=c(-2.5, 0, 0.1), dim2=c(0, -0.2, -1.7))
#' find_next_mv_custom(theta=theta, item_params=item_params, included_variables = included_variables, party_pos=party_pos)


find_next_mv_custom <- function(theta, item_params, included_variables=NULL, party_pos){
  all_next <- list()
  for (i in 1:length(theta)){
    this_next <- find_next_custom(theta[i], item_params[[i]],
                                  included_variables=included_variables[[i]])
    all_next <- c(all_next, list(this_next))
  }
  next_item_dim <- determine_next_dim(all_next, party_pos=party_pos)
  next_item_index <- all_next[[next_item_dim]]$index
  new_info_per_dim <- sapply(all_next, function(q) q$old_itemset_info)
  new_info_per_dim[[next_item_dim]] <- all_next[[next_item_dim]]$new_itemset_info
  list(result = list(next_item_dim = next_item_dim,
                     next_item_index = next_item_index,
                     new_info_per_dim = new_info_per_dim),
       full_result = all_next)
}

# Given the information about each dimension from the find_next function,
# determine the next dimension, from which we select the next question item.
determine_next_dim <- function(all_next, party_pos){
  old_info <- sapply(all_next, function(q) q$old_itemset_info)
  old_var <- 1 / old_info
  new_var <- sapply(all_next, function(q) 1/q$new_itemset_info)
  sd_diff <- sqrt(old_var) - sqrt(new_var)
  importance_scores <- calculate_dim_importance(theta=theta, party_pos=party_pos)$dim_weight
  weighted_importance_scores <-
    weigh_importance_scores(importance_scores=importance_scores,
                               min_info=min(old_info))
  weighted_sd_diff <- sd_diff * weighted_importance_scores
  highest_index <- which.max(weighted_sd_diff)
  if(length(highest_index > 1)) {
    best_highest_index <- highest_index[which.min(new_var[highest_index])]
    return(best_highest_index)
  }
  return(highest_index)
}
