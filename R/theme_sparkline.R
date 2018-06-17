theme_sparkline <- function (base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.background = element_rect(fill = "white",colour = NA),
          panel.grid =element_blank(),
          axis.line.y = element_blank(),
          panel.border = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_text(angle = 0),
          strip.background = element_blank())
}