\name{stat_ma}
\alias{stat_ma}
\alias{StatMa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stat for moving avarage
}
\description{
Stat for moving avarage
}
\usage{
stat_ma(mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  windowsize = NULL,
  stepsize = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{na.rm}{
a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds.
}
  \item{show.legend}{
logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
}
  \item{inherit.aes}{
The position adjustment to use for overlappling points on this layer
}
  \item{windowsize}{
  range to take mean
}
  \item{stepsize}{
  step size
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\examples{
ggplot(economics, aes(date, unemploy)) +
  geom_line()+
  stat_ma(colour="royalblue", windowsize=300)

ggplot(economics, aes(date, unemploy)) +
  geom_line()+
  stat_ma(colour="royalblue", windowsize=300, stepsize=150)
}
