#' Find the next best variable
#'
#' Given a model , the full data set used to train the model and a vector with the column positions of the already included variables, find the next best variable.
#'
#' @param model The mirt model.
#' @param model_data A matrix or data frame with only one row. The column names should be the same as for the data training the model.
#' @param included_variables a vector with the positions of the columns of the included variables. If NULL (default), the current ability is estimated to 0.
#' @return The function returns the name of the next best variable, the position of the column with the next best variable, the total information of the test with the next variable included and the information of each variable.
#' @examples
#'
#' Science_model <- mirt(data = Science, model = 1, itemtype = "graded")
#' find_next(model = Science_model, model_data=Science[1, , drop=F], included_variables = c(1,3))
#'


find_next <- function(model, model_data, included_variables=NULL){
  item_params <- coef(model, IRTpars=T, simplify=T)$items
  if(is.null(included_variables)){
    current_ability <- 0
    current_SE <- NA
  }
  else{
    newdata <- model_data[, included_variables, drop=F]
    current_ability_SE <- est_abilities_mirt(model=model, newdata=newdata, the_method="MAP")
    current_ability <- current_ability_SE$est_theta
    current_SE <- current_ability_SE$SE
  }
  all_info <- grm_info(theta = current_ability, item_params = item_params)
  all_info_new <- all_info
  all_info_new[included_variables] <- 0
  best_info_index <- which.max(all_info_new)
  new_included <- c(included_variables, best_info_index)
  old_itemset_info <- ifelse(is.null(included_variables), 0, sum(all_info[included_variables]))
  new_item_info <- max(all_info_new)
  new_itemset_info <- sum(all_info[new_included])
  best_info_varname <- colnames(model_data)[best_info_index]
  res_list <- list(name=best_info_varname, index=best_info_index,
       old_itemset_info=old_itemset_info,
       new_item_info=new_item_info,
       new_itemset_info=new_itemset_info,
       all_info=all_info,
       estimated_old_ability=current_ability,
       estimated_old_SE=current_SE
       )
  if(length(included_variables) == nrow(item_params)) return(no_vars_left(res_list))
  return(res_list)
}

no_vars_left <- function(res_list){
  res_list$name <- NA
  res_list$index <- NA
  res_list$new_item_info <- NA
  res_list$new_itemset_info <- res_list$old_itemset_info
  return(res_list)
}
