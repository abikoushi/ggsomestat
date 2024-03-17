
StatBinomCI <- ggplot2::ggproto("StatBinomCI", ggplot2::Stat,
                       required_aes = c("x", "numerator", "denominator"),

                       default_aes = aes(ymin = after_stat(ymin),
                                         ymax = after_stat(ymax)),

                       setup_params = function(data, params) {
                         params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)

                         has_x <- !(is.null(data$x) && is.null(params$x))
                         if (!has_x) {
                           abort("stat_binomCI() requires an x aesthetic.")
                         }

                         params
                       },

                       compute_group = function(data, scales, conf.level, flipped_aes = FALSE) {
                         data <- flip_data(data, flipped_aes)
                         #d <<- data
                         CI <-t(mapply(function(x,n)binom.test(x=x, n=n,
                                                               conf.level = conf.level)$conf.int,
                                       data$numerator,
                                       data$denominator))

                         #ci <<- CI
                         data$ymin = CI[,1]
                         data$ymax = CI[,2]

                         flip_data(data, flipped_aes)
                       }
)

#' @export
stat_binomCI <- function(mapping = NULL, data = NULL, geom = "linerange",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, conf.level=0.95,...) {
  layer(
    stat = StatBinomCI, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(conf.level = conf.level, na.rm = na.rm, ...)
  )
}
