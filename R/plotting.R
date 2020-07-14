#' Produce a \code{ggplot2} visualisation of a matrix.
#'
#' @param M A matrix.
#' @return A \code{ggplot2} plot.
#' @examples
#' M <- matrix(1:9, nrow = 3, ncol = 3)
#' plot_matrix(M)
#'
#' plot_matrix(border_precision(mw))
plot_matrix <- function(M){
  M <- t(apply(M, 2, rev)) # Undo 90 degree CC rotation
  ggplot2::ggplot(reshape2::melt(M), aes(x = Var1, y = Var2, fill = value)) +
    labs(title = paste0("Visualise matrix: ", deparse(substitute(M))),
         x = "", y = "", fill = "Value") +
    geom_tile() + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}
