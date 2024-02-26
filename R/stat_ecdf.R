###
# utilities
"%||%" <- ggplot2:::"%||%"
data_frame0 <- ggplot2:::data_frame0
###
#' @export
stat_ecdf2 <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      ...,
                      height = 1,
                      pad = TRUE,
                      decreasing = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEcdf2,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      height = height,
      decreasing = decreasing,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}

StatEcdf2 <- ggproto("StatEcdf2", Stat,
                    required_aes = c("x","y"),

                    default_aes = aes(y = after_stat(y)),

                    setup_params = function(data, params) {
                      params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)

                      has_x <- !(is.null(data$x) && is.null(params$x))
                      has_y <- !(is.null(data$y) && is.null(params$y))
                      if (!has_x || !has_y) {
                        abort("stat_ecdf2() requires an x and y aesthetic.")
                      }

                      params
                    },

                    compute_group = function(data, scales, height = 1, decreasing = FALSE, pad = TRUE, flipped_aes = FALSE) {
                      data <- flip_data(data, flipped_aes)
                      x <- data$x
                      if(pad){
                        x <- c(-Inf,data$x,Inf)
                      }
                      res <- sort(x, decreasing = decreasing)
                      p <- seq(0, 1, length.out = length(x))
                      df_ecdf <- data_frame0(
                        x = unname(res),
                        y = data$y[is.finite(data$y)][1] + height*p,
                        .size = length(res)
                      )
                      #df_ecdf <- new_data_frame(list(x = unname(unlist(res)), y = data$y[is.finite(data$y)][1] + height*p))
                      df_ecdf$flipped_aes <- flipped_aes
                      flip_data(df_ecdf, flipped_aes)
                    }
)

