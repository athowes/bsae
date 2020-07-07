#' Produce a \code{ggplot2} visualisation of a matrix.
#'
#' @param M A matrix.
#' @return A \code{ggplot2} plot.
#' @examples
#' M <- matrix(1:9, nrow = 3, ncol = 3)
#' matrix_plot(M)
#'
#' matrix_plot(border_precision(mw))
matrix_plot <- function(M){
  ggplot(reshape2::melt(M), aes(x = Var1, y = Var2, fill = value)) +
    labs(title = paste0("Visualise matrix: ", deparse(substitute(M))),
         x = "", y = "", fill = "Value") +
    geom_tile()
}
