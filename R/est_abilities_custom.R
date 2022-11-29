#' Estimate the abilities given the item parameters and the test taker responses.
#'
#' @param item_params A matrix of the item parameters. Each row represents one question item. The first column contains the discrimination parameter and the subsequent columns contain the difficulty parameters.
#' @param newdata A data frame or matrix with the response pattern to be evaluated. The number of items should equal the the number of the items in the matrix used as item_params.
#' @param method Can be 'ML' or 'MAP'. 'ML' is default.
#' @param norm_mean A vector of means for a custom normal prior. It should have the same length as there are observations in newdata.
#' @param norm_sd A vector of std devs for a custom normal prior. It should have the same length as there are observations in newdata.
#' @return A vector of length N, where N is the number of test takers to evaluate.
#' @examples
#' Science <- mirt::Science
#' mirtmodel <- mirt(data=Science, model=1, itemtype = "graded")
#' my_item_params <- coef(mirtmodel, IRTpars=T, simplify=T)$items
#' est_abilities_custom(item_params=my_item_params, newdata=Science, the_method="ML")

est_abilities_custom <- function(item_params, newdata, the_method="ML",
                                 norm_mean=NULL, norm_sd=NULL){
  onehot_all <- encode_onehot(newdata=newdata, item_params = item_params)
  est_abilities <- c()
  est_SE <- c()
  for(i in 1:nrow(newdata)){
    this_norm_mean <- ifelse(is.null(norm_mean), 0, norm_mean[i])
    this_norm_sd <- ifelse(is.null(norm_sd), 1, norm_sd[i])
    this_onehot <- t(onehot_all[i, , ])
    opt_value <- optim(par=0, fn=log_post_custom, gr=NULL,
          item_params=item_params, onehot_response=this_onehot,
          the_method=the_method,
          norm_mean=this_norm_mean, norm_sd=this_norm_sd,
          method="BFGS", control=list(fnscale=-1), hessian=TRUE)
    est_abilities <- c(est_abilities, opt_value$par)
    this_SE <- sqrt(1 / -c(opt_value$hessian))
    est_SE <- c(est_SE, this_SE)
  }
  list(est_theta=est_abilities, SE=est_SE)
}

log_post_custom <- function(theta, item_params, onehot_response, the_method="ML",
                            norm_mean=0, norm_sd=1){
  a <- item_params[, 1]
  d <- item_params[, -1, drop=F]
  nl <- ncol(item_params)
  z <- a * (theta - d)
  p_cum <- cbind(1, plogis(q = z), 0)
  if(nrow(item_params) == 1){
    p <- diff(p_cum[, (nl+1):1])[nl:1] |> matrix(nrow=1)
  }else{
    p <- t(diff(t(p_cum[, (nl+1):1])))[, nl:1]}
  log_like <- sum(log(rowSums(p * onehot_response)))
  log_prior <- ifelse(the_method == "MAP",
                      dnorm(theta, mean=norm_mean, sd=norm_sd, log = T), 0)
  log_like + log_prior
}

encode_onehot <- function(newdata, item_params){
  nr <- nrow(newdata)
  nq <- ncol(newdata)
  nl <- ncol(item_params)
  onehot_array <- array(NA, dim=c(nr, nl, nq))
  for (i in 1:nq) onehot_array[, , i] <-
    sapply(1:nl, function(q) as.numeric(newdata[, i] == q))
  onehot_array
}



