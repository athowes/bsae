#' HIV Survey Data.
#'
#' A data frame containing HIV survey data from 38 surveys across 19 African countries.
#'
#' @format A simple features data frame with 2435 rows and 13 variables:
#' * \code{survey_id}
#' * \code{gid_0}
#' * \code{gid_1}
#' * \code{gid_2}
#' * \code{name_0}
#' * \code{name_1}
#' * \code{name_2}
#' * \code{n_cluster}
#' * \code{n_obs}
#' * \code{est}
#' * \code{se}
#' * \code{y}
#' * \code{geometry}
#' @source Jeff Eaton
"hiv_surveys"

#' Malawi DHS 2015 Survey Data.
#'
#' \code{hiv_surveys} subsetted to just the DHS 2015 survey in Malawi.
#'
#' @format A simple features data frame with 28 rows and 13 variables.
#' @source Jeff Eaton
"mw"

#' Model lookup table.
#'
#' Used for reference and in the function \code{\link{get_model_method}}.
"lookup_model"

#' Method lookup table.
#'
#' Used for reference and in the function \code{\link{get_model_method}}.
"lookup_method"
