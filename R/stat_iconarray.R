data_frame0 <- ggplot2:::data_frame0

# make_countgrid <- function(data, freq, n_col=10L){
#   data <- data[rep(1:nrow(data), freq),]
#   row.names(data) <- NULL
#   x <- (0L:(nrow(data)-1L)) %/% n_col +1L
#   y <- 1L:nrow(data) %% n_col
#   y[y==0L] <- n_col
#   data$x <- x
#   data$y <- y
#   return(data)
# }

stat_countgrid <- function(mapping = NULL, data = NULL,
                           geom = "point", position = "identity",
                           ...,
                           n_col = 10,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCountgrid,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n_col = n_col,
      ...
    )
  )
}

StatCountgrid <- ggproto("StatCountgrid",
                         Stat,
                         required_aes = c("x"),

                         default_aes = aes(x = after_stat(x), y = after_stat(y)),

                        setup_data =function(data, params){
                          #dd <<- data
                            data <- data[rep(1:nrow(data), data$x),]
                            row.names(data) <- NULL
                            data$group <- 1
                            return(data)
                        },

                         compute_group = function(data, scales, n_col = 10L, flipped_aes = FALSE) {
                           data <- flip_data(data, flipped_aes)
                           #data <- make_countgrid(data, data$x, n_col=n_col)
                           x <- (0L:(nrow(data)-1L)) %/% n_col +1L
                           y <- 1L:nrow(data) %% n_col
                           y[y==0L] <- n_col
                           data$x <- x
                           data$y <- y
                           #d <<- data
                           flip_data(data, flipped_aes)
                         }
)
