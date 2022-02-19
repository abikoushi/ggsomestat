\name{stat_binomCI}
\alias{stat_binomCI}
\alias{StatBinomCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute confidence interval of a parameter of the binomial distribution
}
\description{
Draw confidence interval of a parameter of the binomial distribution.
}
\usage{
stat_binomCI(mapping = NULL, data = NULL, geom = "linerange", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, conf.level = 0.95, ...)
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
  \item{na.rm}{
  If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
}
  \item{show.legend}{
logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
}
  \item{inherit.aes}{
The position adjustment to use for overlappling points on this layer
}
  \item{conf.level}{
confidence level for the returned confidence interval.
}
  \item{\dots}{
other arguments passed on to layer. This can include aesthetics whose values you want to set, not map. See layer for more details.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
entry0117 <- searchConsole[searchConsole$pagePath=="/entry/2015/01/17/064522",]
ggplot(entry0117, aes(date, clicks/impressions)) +
  geom_line()+
  stat_binomCI(geom = "ribbon",aes(numerator=clicks,denominator=impressions),alpha=0.3)
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
