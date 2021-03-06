% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fik.R
\name{fik_stan}
\alias{fik_stan}
\title{Fit Integrated MVN Small Area Estimation model using \code{rstan}.}
\usage{
fik_stan(
  sf,
  bym2 = FALSE,
  L = 10,
  type = "hexagonal",
  nsim_warm = 100,
  nsim_iter = 1000,
  chains = 4,
  cores = parallel::detectCores(),
  kernel = matern,
  ...
)
}
\arguments{
\item{sf}{A simple features object with some geometry.}

\item{bym2}{Logical indicating if the spatial random effects should be convoluted
with unstructured IID noise, defaults to \code{FALSE}.}

\item{L}{The number of Monte Carlo samples to draw from each area.}

\item{type}{The \code{type} argument of \code{sf::st_sample}, defaults to \code{"hexagonal"}}

\item{nsim_warm}{Number of warmup samples, passed to \code{rstan}.}

\item{nsim_iter}{Number of samples, passed to \code{rstan}.}

\item{chains}{Number of chains, each of which gets \code{nsim_warm + nsim_iter} samples, passed to \code{rstan}.}

\item{cores}{Number of cores, passed to \code{rstan}, defaults to \code{parallel::detectCores()}}

\item{kernel}{A kernel function, defaults to \code{matern}.}

\item{...}{Additional arguments to \code{kernel}.}
}
\description{
Random effects have a multivariate Gaussian distribution with covariance
matrix calculated using \code{\link{integrated_covariance}}.
}
\examples{
fik_stan(mw, nsim_warm = 0, nsim_iter = 100, cores = 2)
}
