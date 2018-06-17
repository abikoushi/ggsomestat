#' @import ggplot2
#' @export
GeomLasagna <- ggplot2::ggproto("GeomLasagna", ggplot2::GeomRect,
                       extra_params = c("na.rm", "or_more"),
                       required_aes = c("x", "y","fill"),
                       setup_data = function(data, params) {
                         if (!is.na(params$or_more)) {
                           y2 <- with(data, ifelse(data$y >= params$or_more, paste0(params$or_more,"+"),data$y))
                           lev <-sort(unique(data$y[data$y < params$or_more]))
                           lev <-c(lev, paste0(params$or_more,"+"))
                           y2 <- factor(y2, levels = lev)
                           out=aggregate(fill~x+y2+PANEL+group,sum,data=data)
                           out$y =out$y2
                           data <-out
                         }
                         data =transform(data,
                                   xmin = x - 1/2,  xmax = x + 1/2,
                                   ymin = as.integer(y) - 1/2, ymax = as.integer(y) + 1/2
                         )
                         data
                       }
                       
)

geom_lasagna <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE, na.rm=TRUE, or_more = NA,...) {
  list(layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLasagna,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm=na.rm, or_more = or_more,...)
  ),
  scale_y_discrete())
}

GeomLasagnaText <- ggplot2::ggproto("GeomLasagnaText", ggplot2::GeomText,
                           extra_params = c("na.rm", "or_more"),
                           required_aes = c("x", "y","label"),
                           setup_data = function(data, params) {
                             if (!is.na(params$or_more)) {
                               y2 <- with(data, ifelse(data$y >= params$or_more, paste0(params$or_more,"+"),data$y))
                               lev <-sort(unique(data$y[data$y < params$or_more]))
                               lev <-c(lev, paste0(params$or_more,"+"))
                               y2 <- factor(y2, levels = lev)
                               out=aggregate(label~x+y2+PANEL+group,sum,data=data)
                               out$y =out$y2
                               data <-out
                               #print(head(data))
                             }
                             data
                           }
                           
)

GeomLasagnaLabel <- ggplot2::ggproto("GeomLasagnaText", ggplot2::GeomLabel,
                            extra_params = c("na.rm", "or_more"),
                            required_aes = c("x", "y","label"),
                            setup_data = function(data, params) {
                              if (!is.na(params$or_more)) {
                                y2 <- with(data, ifelse(data$y >= params$or_more, paste0(params$or_more,"+"),data$y))
                                lev <-sort(unique(data$y[data$y < params$or_more]))
                                lev <-c(lev, paste0(params$or_more,"+"))
                                y2 <- factor(y2, levels = lev)
                                out=aggregate(label~x+y2+PANEL+group,sum,data=data)
                                out$y =out$y2
                                data <-out
                                #print(head(data))
                              }
                              data
                            }
                            
)

geom_lasagna_text <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", show.legend = NA,
                              inherit.aes = TRUE, na.rm=TRUE, or_more = NA,...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLasagnaText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm=na.rm, or_more = or_more,...)
  )
}

geom_lasagna_label <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", show.legend = NA,
                               inherit.aes = TRUE, na.rm=TRUE, or_more = NA,...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLasagnaLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm=na.rm, or_more = or_more,...)
  )
}