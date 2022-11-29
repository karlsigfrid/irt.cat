#' Estimate the abilities given a model
#'
#' Estimate the abilities given a MIRT model, using all or a subset of the items used to train the model..
#'
#' @param model The mirt model.
#' @param newdata A data frame or matrix with the response pattern to be evaluated. The column names in newdata must be column names that are used in the dataset with which the MIRT model is trained.
#' @param the_metod "ML" or "MAP". "ML" is default.
#' @return A vector of length N, where N is the number of test takers to evaluate.
#' @examples
#' data(Science)
#' Science_model <- mirt(data = Science, model = 1, itemtype = "graded")
#' est_abilities_mirt(model = Science_model, newdata = Science[1:2, c(1, 3)])


est_abilities_mirt <- function(model, newdata, the_method="ML"){
  require(dplyr)
  all_vars <- colnames(model@Data$data)
  included_vars <- colnames(newdata)
  data <- matrix(NA, nrow=nrow(newdata), ncol=ncol(model@Data$data)) %>%
    data.frame
  colnames(data) <- all_vars
  data[, included_vars] <- newdata
  this_fit <- fscores(model, method=the_method, response.pattern = data)
  list(est_theta=this_fit[, 1], SE=this_fit[, 2])
}
