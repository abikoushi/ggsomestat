\name{stat_ecdf2}
\alias{stat_ecdf2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute empirical cumulative distribution
}

\usage{
stat_ecdf2(mapping = NULL, data = NULL, geom = "step", position = "identity", ..., height = 1, pad = TRUE, decreasing = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
%%     ~~Describe \code{mapping} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{geom}{
%%     ~~Describe \code{geom} here~~
}
  \item{position}{
%%     ~~Describe \code{position} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{pad}{
%%     ~~Describe \code{pad} here~~
}
  \item{decreasing}{
%%     ~~Describe \code{decreasing} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{show.legend}{
%%     ~~Describe \code{show.legend} here~~
}
  \item{inherit.aes}{
%%     ~~Describe \code{inherit.aes} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mapping = NULL, data = NULL, geom = "step", position = "identity",
    ..., height = 1, pad = TRUE, decreasing = FALSE, na.rm = FALSE,
    show.legend = NA, inherit.aes = TRUE)
{
    layer(data = data, mapping = mapping, stat = StatEcdf2, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(height = height, decreasing = decreasing,
            pad = pad, na.rm = na.rm, ...))
  }
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
