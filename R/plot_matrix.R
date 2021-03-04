#' Produce a `ggplot2` visualisation of a matrix.
#'
#' @param M A matrix.
#' @return A `ggplot2` plot.
#' @examples
#' M <- matrix(1:9, nrow = 3, ncol = 3)
#' plot_matrix(M)
#'
#' plot_matrix(nb_to_precision(neighbours(mw)))
#' @export
plot_matrix <- function(M){
  M <- t(apply(M, 2, rev)) # Undo 90 degree CC rotation
  ggplot2::ggplot(reshape2::melt(M), ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = .data$value)) +
    ggplot2::labs(x = "", y = "", fill = "Value") +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
}
