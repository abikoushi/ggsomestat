
StatMA <- ggplot2::ggproto("StatMA", ggplot2::Stat,
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales,
                                           windowsize = NULL,
                                           stepsize = NULL,
                                           origin = NULL,
                                           na.rm = FALSE,
                                           flipped_aes = FALSE) {
                    data <- ggplot2::flip_data(data, flipped_aes)
                    xname <- flipped_names(flipped_aes)$x
                    x <- data$x
                    y <- data$y
                    ran <- range(x)
                    tau <- diff(ran)
                    if(is.null(windowsize)){
                      windowsize <- tau/30
                      cli::cli_inform("{.fn stat_ma} using {.code range(x)/30}. Pick better value with {.arg windowsize}.")
                    }
                    if(is.null(stepsize)){
                      stepsize <- windowsize
                    }
                    if(is.null(origin)){
                      origin <- ran[1]
                    }
                    m <- ceiling((tau-windowsize)/stepsize)
                    x2 <- numeric(m+1)
                    y2 <- numeric(m+1)
                    take <- logical(m+1)
                    for(i in 0:m){
                      f <- origin + i*stepsize <= x & x<= origin + i*stepsize+windowsize
                      if(any(f)){
                        x2[i+1] <- mean(x[f], na.rm=na.rm)
                        y2[i+1] <- mean(y[f], na.rm=na.rm)
                        take[i+1] <- TRUE
                      }
                    }
                    out <- data.frame(x=x2[take], y=y2[take])
                    out$flipped_aes <- flipped_aes
                    flip_data(out, flipped_aes)
                    # grid <- data.frame(x = data$x)
                    # grid$y <- as.numeric(stats::filter(data$y, rep(1,windowsize)) / windowsize)
                    # grid[!is.na(grid$y),]
                  }
)


stat_ma <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, windowsize = NULL, stepsize = NULL, ...) {
  layer(
    stat = StatMA, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(windowsize = windowsize, stepsize = stepsize, na.rm = na.rm, ...)
  )
}
