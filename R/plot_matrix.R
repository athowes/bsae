#' Produce a `ggplot2` visualisation of a matrix.
#'
#' @param M A matrix.
#' @return A `ggplot2` plot.
#' @examples
#' M <- matrix(1:9, nrow = 3, ncol = 3)
#' plot_matrix(M)
#'
#' plot_matrix(border_precision(mw))
#' @export
plot_matrix <- function(M){
  M <- t(apply(M, 2, rev)) # Undo 90 degree CC rotation
  ggplot2::ggplot(reshape2::melt(M), ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::labs(x = "", y = "", fill = "Value") +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
}
