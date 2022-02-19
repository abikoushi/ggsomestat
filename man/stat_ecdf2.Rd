\name{stat_ecdf2}
\alias{stat_ecdf2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute empirical cumulative distribution
}

\usage{
stat_ecdf2(mapping = NULL,
  data = NULL,
  geom = "step",
  position = "identity",
  ...,
  height = 1,
  pad = TRUE,
  decreasing = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
}
  \item{data}{
The data to be displayed in this layer.
}
  \item{geom}{
The geometric object to use display the data
}
  \item{position}{
Position adjustment, either as a string, or the result of a call to a position adjustment function.
}
  \item{\dots}{
other arguments passed on to layer. This can include aesthetics whose values you want to set, not map. See layer for more details.
}
  \item{height}{
Max height. By default, set to 1.
}
  \item{pad}{
If \code{TRUE}, pad the ecdf with additional points \code{-Inf} and \code{Inf}
}
  \item{decreasing}{
If \code{TRUE}, the ecdf shows upper probabilities. By default, set to \code{FALSE}.
}
  \item{na.rm}{
  If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
}
  \item{show.legend}{
logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
}
\item{inherit.aes}{
The position adjustment to use for overlappling points on this layer
}
}
\examples{
ggplot(searchConsole, aes(impressions, pagePath)) +
  stat_ecdf2()
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
