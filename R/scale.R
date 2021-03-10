#' Compute the Riebler generalised variance of a covariance matrix.
#' 
#' Let \eqn{A} be a square matrix, then the Riebler generalised variance
#' is defined as the geometric mean of the marginal variances, given by
#' \deqn{\sigma_{\mathrm{GV}}^2(A) = \exp \left( \frac{1}{n} \sum_{i = 1}^n \log A_{ii} \right).}
#'
#' @param A A square matrix.
#' @export
riebler_gv <- function(A) {
  exp(mean(log(diag(A))))
}

#' Compute scale of a precision matrix using `R-INLA`.
#' 
#' Takes the [`riebler_gv`] of the inverse of the precision. 
#' If the precision is singular, then the generalised inverse is used as implemented by `INLA::inla.qinv`, 
#' and by default it is assumed that the graph is fully connected.
#' 
#' @param Q A (square, symmetric) precision matrix.
#' @param constraint A list with arguments `A` and `e` which imposes the constraint `Au = e`
#' (where the precision of `u` is `Q`). See the `?INLA::f` argument `extraconstr`. 
#' If `constraint` is the default value then this is a sum-to-zero constraint.
#' @examples
#' nb <- neighbours(mw)
#' Q <- nb_to_precision(nb)
#' get_scale(Q)
#' @export
get_scale <- function(Q, constraint = list(A = matrix(1, 1, nrow(Q)), e = 0)){
  n <- nrow(Q)
  # Add jitter to the diagonal for numerical stability
  Q_prt <- Q + Matrix::Diagonal(n) * max(diag(Q)) * sqrt(.Machine$double.eps)
  if(det(Q) == 0) {
    # Inversion of sparse, singular matrix
    Q_inv <- INLA::inla.qinv(Q_prt, constr = constraint)
  }
  else {
   Q_inv <- solve(Q) 
  }
  # Compute the GV on the covariance matrix
  return(riebler_gv(as.matrix(Q_inv)))
}

#' Scales the precision matrix of a Gaussian Markov random field.
#' 
#' Implements the same thing as `INLA::inla.scale.model`.
#' 
#' @inheritParams get_scale
#' @param A See [`get_scale`].
#' @return A list containing a rescaled (see "A note on intrinsic conditional autoregressive models for 
#' disconnected graphs" Freni-Sterrantino, Ventrucci and Rue) matrix `Q`, and `scales` containing the
#' of the scale used for each connected component.
#' @source From \href{https://github.com/mrc-ide/naomi/blob/master/R/car.R}{code} by Jeff Eaton.
#' @examples
#' nb <- neighbours(mw)
#' Q <- nb_to_precision(nb)
#' scale_gmrf_precision(Q)
#' @export
scale_gmrf_precision <- function(Q, A = matrix(1, 1, nrow(Q))){
  nb <- spdep::mat2listw(abs(Q))$neighbours
  comp <- spdep::n.comp.nb(nb)
  scales <- rep(NA, comp$nc)
  for (k in seq_len(comp$nc)) {
    idx <- which(comp$comp.id == k)
    Qc <- Q[idx, idx, drop = FALSE]
    if (length(idx) == 1) {
      scales[k] <- 1 
      Qc[1, 1] <- 1 # Set marginal variance for islands to be 1
    } else {
      Ac <- A[ , idx, drop = FALSE]
      scale <- get_scale(Qc, constraint = list(A = Ac, e = 0))
      scales[k] <- scale
      Qc <- scale * Qc
    }
    Q[idx, idx] <- Qc
  }
  return(list(Q = Q, scales = scales))
}