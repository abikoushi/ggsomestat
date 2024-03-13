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
stat_countgrid(mapping = NULL, data = NULL,
  geom = "point", position = "identity", ...,
  n_col = 10, show.legend = NA, inherit.aes = TRUE)
}
\arguments{
  \item{mapping}{
  Set of aesthetic mappings created by \code{\link[=aes]{aes()}}.
}
  \item{data}{
  data to be displayed in this layer
}
  \item{geom}{
  geometric object to use to display the data
}
  \item{position}{
  Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter})
}
  \item{n_col}{number of rows for grid}
    \item{show.legend}{
logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
}
  \item{inherit.aes}{
The position adjustment to use for overlappling points on this layer
}
}
\examples{
library(ggsomestat)
HEC <- as.data.frame(HairEyeColor)
ggplot(HEC, aes(y=Freq, colour=Sex))+
  stat_countgrid(shape=15)+
  facet_grid(Hair~Eye, labeller = label_both)
}
