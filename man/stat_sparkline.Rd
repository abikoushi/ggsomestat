\name{stat_sparkline}
\alias{stat_sparkline}
\alias{StatSparkline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw sparkline
}
\usage{
stat_sparkline(
  mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
The aesthetic mapping, usually constructed with aes or aes_string. Only needs to be set at the layer level if you are overriding the plot defaults.
}
  \item{data}{
A layer specific dataset - only needed if you want to override the plot defaults.
}
  \item{geom}{
The geometric object to use display the data
}
  \item{position}{
The position adjustment to use for overlappling points on this layer
}
  \item{\dots}{
other arguments passed on to layer. This can include aesthetics whose values you want to set, not map. See layer for more details.
}

  \item{na.rm}{
  If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
}
  \item{orientation}{
  The orientation of the layer. The default (NA) automatically determines the orientation from the aesthetic mapping. In the rare event that this fails it can be given explicitly by setting orientation to either "x" or "y". See the Orientation section for more detail.
}
  \item{show.legend}{
logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
}
  \item{inherit.aes}{
If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. \code{borders()}.
}
}
\examples{
ggplot(searchConsole, aes(x = date, y = pagePath, inner_y = clicks)) +
  stat_sparkline()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
