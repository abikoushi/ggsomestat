\name{stat_countgrid}
\alias{stat_countgrid}
\alias{StatCountgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Icon array
}
\description{
Arrange objects for the number entered `x`
}
\usage{
stat_countgrid(mapping = NULL, data = NULL, geom = "point", position = "identity", ..., n_col = 10, show.legend = NA, inherit.aes = TRUE)
}
\arguments{
  \item{n_col}{number of rows}
}
\examples{
library(ggsomestat)
HEC <- as.data.frame(HairEyeColor)
ggplot(HEC, aes(y=Freq, colour=Sex))+
  stat_countgrid(shape=15)+
  facet_grid(Hair~Eye, labeller = label_both)
}
