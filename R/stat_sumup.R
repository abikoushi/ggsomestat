#' @import ggplot2
#' @export
StatSumUp <- ggplot2::ggproto("StatSomeUp", ggplot2::Stat,
                  required_aes = c("x", "y"),

                  compute_group = function(data, scales, or_more) {
                    if (!is.na(or_more)) {
                      y2 <- with(data, ifelse(data$y >= or_more, paste0(or_more,"~"),data$y))
                      lev <-sort(unique(data$y[data$y < or_more]))
                      lev <-c(lev, paste0(params$or_more,"+"))
                      y2 <- factor(y2, levels = lev)
                      out=aggregate(label~x+y2+PANEL+group,sum,data=data)
                      out$y =out$y2
                      data <-out
                    }
                    data
                  }
)

stat_sumup <- function(mapping = NULL, data = NULL, geom = "tile",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, or_more = NA, ...) {
  layer(
    stat = StatMA, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(or_more = or_more, na.rm = na.rm, ...)
  )
}
