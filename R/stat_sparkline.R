###
#rbind_dfs <- ggplot2:::rbind_dfs
#new_data_frame <- ggplot2:::new_data_frame
#dapply <- ggplot2:::dapply
#df_rows <- ggplot2:::df_rows
#modify_list <- ggplot2:::modify_list
#id_var <- ggplot2:::id_var
#id <- ggplot2:::id
###
# utilities
"%||%" <- ggplot2:::"%||%"
data_frame0 <- ggplot2:::data_frame0
###

#' @export
stat_sparkline <- function(mapping = NULL, data = NULL,
                       geom = "line", position = "identity",
                       ...,
                       height = 0.9,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSparkline,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      height = height,
      na.rm = na.rm,
      ...
    )
  )
}

StatSparkline <- ggproto("StatSparkline", Stat,
                     required_aes = c("x","y","inner_y"),

                     default_aes = aes(y = after_stat(y)),

                     setup_params = function(data, params) {
                       params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)

                       has_x <- !(is.null(data$x) && is.null(params$x))
                       has_y <- !(is.null(data$y) && is.null(params$y))
                       if (!has_x || !has_y) {
                         abort("stat_sparkline() requires an x and y aesthetic.")
                       }

                       params
                     },

                     compute_group = function(data, scales, height = 1, flipped_aes = FALSE) {
                       data <- flip_data(data, flipped_aes)
                       y <- data$inner_y
                       y <- y/max(abs(y),na.rm = TRUE)
                       mp = median(y, na.rm = TRUE)
                       df_sp <- data_frame0(x = data$x,
                                            y = data$y + height*(y-mp),
                                            .size = length(x))
                       #df_sp <- new_data_frame(list(x = data$x, y = data$y + height*(y-mp)))
                       df_sp$flipped_aes <- flipped_aes
                       flip_data(df_sp, flipped_aes)
                     }
)

