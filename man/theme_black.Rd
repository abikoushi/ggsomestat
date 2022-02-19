\name{theme_black}
\alias{theme_bwblack}
\alias{theme_miniblack}
\alias{theme_classicblack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Black themes
}
\description{
The default theme for ggplot2 is gray letters, but these are black.　Use \code{theme()} if you just need to tweak the display of an existing theme.
}
\usage{
theme_bwblack(
  base_size = 13,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
)

theme_miniblack(
  base_size = 13,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
)

theme_classicblack(
  base_size = 13,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base_size}{base font size, given in pts.}
  \item{base_family}{base font family}
  \item{base_line_size}{base size for line elements}
  \item{base_rect_size}{base size for rect elements}
}
\examples{
ggplot(economics, aes(date, unemploy)) +
  geom_line()+
  stat_ma(colour="royalblue", windowsize=30)+
  theme_bwblack()

ggplot(searchConsole, aes(x = date, y = pagePath, inner_y = clicks)) +
  stat_sparkline()+
  theme_miniblack()

entry0117 <- searchConsole[searchConsole$pagePath=="/entry/2015/01/17/064522",]
ggplot(entry0117, aes(date, clicks/impressions)) +
  geom_line()+
  stat_binomCI(geom = "ribbon", aes(numerator=clicks, denominator=impressions), alpha=0.3)+
  theme_classicblack()
}
