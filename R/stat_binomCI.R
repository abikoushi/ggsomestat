#' @import ggplot2
#' @export
StatBinomCI <- ggplot2::ggproto("StatBinomCI", ggplot2::Stat,
                       required_aes = c("x", "numerator", "denominator"),

                       compute_group = function(data, scales, conf.level) {
                         CI <-t(mapply(function(x,n)binom.test(x=x, n=n, conf.level = conf.level)$conf.int,
                                       data$numerator,
                                       data$denominator))
                         grid <-as.data.frame(CI)
                         colnames(grid) <- c("ymin","ymax")
                         grid$x <- data$x
                         grid
                       }
)
stat_binomCI <- function(mapping = NULL, data = NULL, geom = "linerange",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, conf.level=0.95,...) {
  layer(
    stat = StatBinomCI, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(conf.level = conf.level, na.rm = na.rm, ...)
  )
}
