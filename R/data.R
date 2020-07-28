#' HIV Survey Data.
#'
#' A data frame containing HIV survey data from 38 surveys across 19 African countries.
#'
#' @format A simple features data frame with 2435 rows and 13 variables:
#' * `survey_id`
#' * `gid_0`
#' * `gid_1`
#' * `gid_2`
#' * `name_0`
#' * `name_1`
#' * `name_2`
#' * `n_cluster`
#' * `n_obs`
#' * `est`
#' * `se`
#' * `y`
#' * `geometry`
#' @source Jeff Eaton
"hiv_surveys"

#' Malawi DHS 2015 Survey Data.
#'
#' `hiv_surveys` subsetted to just the DHS 2015 survey in Malawi.
#'
#' @format A simple features data frame with 28 rows and 13 variables.
#' @source Jeff Eaton
"mw"

#' Model lookup table.
#'
#' Used for reference and in the function [`get_model_method`].
"lookup_model"

#' Method lookup table.
#'
#' Used for reference and in the function [`get_model_method`].
"lookup_method"
