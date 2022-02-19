#' @import ggplot2
#' @export
theme_bwblack <- function (base_size = 13, base_family = "", base_line_size = base_size/22,
                       base_rect_size = base_size/22) {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family,
                      base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(axis.text.x = element_text(colour="black",angle=90,hjust = 1),
                   axis.text.y = element_text(colour="black"),
                   panel.background = element_rect(fill = "white",colour = NA),
                   panel.border = element_rect(fill = NA,colour = "grey20"),
                   panel.grid = element_line(colour = "grey92"),
                   panel.grid.minor = element_line(size = rel(0.5)),
                   strip.background = element_rect(fill = "grey85",colour = "grey20"),
                   legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)
}

theme_miniblack <- function (base_size = 13, base_family = "", base_line_size = base_size/22,
                             base_rect_size = base_size/22) {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family,
                      base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(legend.background = element_blank(),
                   legend.key = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   strip.background = element_blank(),
                   plot.background = element_blank(),
                   panel.grid = element_line(colour = "grey92"),
                   panel.grid.minor = element_line(size = rel(0.5)),
                   axis.text.x = element_text(colour="black"),
                   axis.text.y = element_text(colour="black"),
                   complete = TRUE)
}

theme_classicblack <- function (base_size = 13, base_family = "", base_line_size = base_size/22,
                                base_rect_size = base_size/22)
{
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black",size = rel(1)), legend.key = element_blank(),
          strip.background = element_rect(fill = "white", colour = "black", size = rel(2)), complete = TRUE)
}
