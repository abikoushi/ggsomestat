\name{stat_ma}
\alias{stat_ma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
moving avarage
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stat_ma(mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  windowsize = 7, ...)
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
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{show.legend}{
%%     ~~Describe \code{show.legend} here~~
}
  \item{inherit.aes}{
%%     ~~Describe \code{inherit.aes} here~~
}
  \item{windowsize}{
%%     ~~Describe \code{windowsize} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
ggplot(economics, aes(date, unemploy)) +
  geom_line()+
  stat_ma(colour="royalblue", windowsize=30)
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
