#' Find the next best variable, choosing from variables in multiple dimensions.
#'
#' Given N models, the full data set used to train each model and N vectors with the column positions of the already included variables, find the next best variable for each dimension.
#'
#' @param model a list of N mirt models.
#' @param model_data A list of N matrices or data frames, each with 1 row. The column names in the matrices/data frames should be the same as for the data training the model.
#' @param included_variables A list of N vectors, each with with the positions of the columns of the included variables in one of the dimensions. If NULL (default), the current ability is estimated to 0.
#' @param party_pos A data frame. Column 1 is the party name. I addition, one column should be included per dimension.
#' @return The function returns the dimension with the next best variable, name of the next best variable, the position of the column with the next best variable, the total information of the test in each dimension with the next variable included. The information in One dimension will be updated, while the information in the other dimensions will remain unchanged.
#' @examples
#'
#' Science_model <- mirt(data = mirt::Science, model = 1, itemtype = "graded")
#' SAT12_model <- mirt(data = mirt::SAT12[, 1:5], model = 1, itemtype = "graded")
#' model <- list(Science_model, SAT12_model)
#' model_data <- list(Science[1, , drop=F], SAT12[1, 1:10, drop=F])
#' included_variables <- list(c(1, 3), NULL)
#' party_pos <- data.frame(party=c("p1", "p2", "p3"), dim1=c(-2.5, 0, 0.1), dim2=c(0, -0.2, -1.7))
#' find_next_mv(model = model, model_data=model_data, included_variables = included_variables)


find_next_mv <- function(model, model_data, included_variables, party_pos){
  all_next <- list()
  for (i in 1:length(model)){
    this_next <- find_next(model=model[[i]], model_data=model_data[[i]],
                           included_variables=included_variables[[i]])
    all_next <- c(all_next, list(this_next))
  }
  next_item_dim <- determine_next_dim(all_next)
  next_item_name <- all_next[[next_item_dim]]$name
  next_item_index <- all_next[[next_item_dim]]$index
  new_info_per_dim <- sapply(all_next, function(q) q$old_itemset_info)
  new_info_per_dim[[next_item_dim]] <- all_next[[next_item_dim]]$new_itemset_info
  list(result = list(next_item_dim = next_item_dim,
                     next_item_name = next_item_name,
                     next_item_index = next_item_index,
                     new_info_per_dim = new_info_per_dim),
       full_result = all_next)
}

# Given the information about each dimension from the find_next function,
# determine the next dimension, from which we select the next question item.
determine_next_dim <- function(all_next, party_pos, mu=4){
  old_info <- sapply(all_next, function(q) q$old_itemset_info)
  old_var <- 1 / old_info
  new_var <- sapply(all_next, function(q) 1/q$new_itemset_info)
  var_diff <- old_var - new_var
  old_theta <- sapply(all_next, function(q) q$estimated_old_ability)
  importance_scores <- calculate_dim_importance(theta=old_theta, party_pos=party_pos)
  no_importance_scores <- rep(1 / length(old_info), length(old_info))
  var_diff_importance <- var_diff * importance_scores
  var_diff_no_importance <- var_diff * no_importance_scores
  importance_weight <- pnorm(q=min(old_info), mean=mu, sd=1)
  weighted_var_diff <- var_diff_no_importance +
    importance_weight * (var_diff_importance - var_diff_no_importance)
  highest_index <- which.max(weighted_var_diff)
  if(length(highest_index > 1)) {
    best_highest_index <- highest_index[which.min(new_var[highest_index])]
    return(best_highest_index)
  }
  return(highest_index)
}
