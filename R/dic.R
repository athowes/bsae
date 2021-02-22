#' Deviance information criterion generic.
#'
#' @param fit Fitted model.
#' @export
dic <- function(fit, ...) {
  UseMethod("dic")
}

#' @rdname dic
#' @export
dic.inla <- function(fit) {
  fit$dic$dic
}

#' @rdname dic
#' @export
dic.stanfit <- function(fit) {
  pointwise_log_lik <- rstan::extract(fit, 'log_lik')$log_lik
  log_lik <- rowSums(pointwise_log_lik)
  mean_deviance <- -2 * mean(log_lik)
  deviance_mle <- -2 * max(log_lik)
  # p_dic <- mean_deviance - deviance_mle
  dic <- 2*mean_deviance - deviance_mle
  return(dic)
}