#' @import ggplot2
#' @export
StatMA <- ggplot2::ggproto("StatMA", ggplot2::Stat,
                  required_aes = c("x", "y"),

                  compute_group = function(data, scales,windowsize) {
                    grid <- data.frame(x = data$x)
                    grid$y <- stats::filter(data$y, rep(1,windowsize)) / windowsize
                    grid
                  }
)

stat_ma <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, windowsize = 7, ...) {
  layer(
    stat = StatMA, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(windowsize = windowsize, na.rm = na.rm, ...)
  )
}
