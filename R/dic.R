#' Deviance information criterion generic.
#'
#' @param fit Fitted model.
#' @param ... Additional arguments passed to `dic`.
#' @export
dic <- function(fit, ...) {
  UseMethod("dic")
}

#' @rdname dic
#' @export
dic.inla <- function(fit) {
  local_dic <- fit$dic$local.dic
  est <- sum(local_dic)
  se <- stats::sd(local_dic) * sqrt(length(local_dic))
  return(list(est = est, se = se))
}

#' @rdname dic
#' @export
dic.stanfit <- function(fit) {
  pointwise_log_lik <- rstan::extract(fit, 'log_lik')$log_lik
  log_lik <- rowSums(pointwise_log_lik)
  mean_deviance <- -2 * mean(log_lik)
  deviance_mle <- -2 * max(log_lik)
  # p_dic <- mean_deviance - deviance_mle
  est <- 2 * mean_deviance - deviance_mle
  se <- NA # To find
  return(list(est = est, se = se))
}