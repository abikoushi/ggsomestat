\name{axis_text_black}
\alias{axis_text_black}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
apply black fonts for axis text
}
\description{
This function is equivalent to \code{ggplot2::theme(axis.text = element_text(colour="black"))}
}
\usage{
axis_text_black()
}
\examples{
ggplot(economics, aes(date, unemploy)) +
  geom_line()+
  axis_text_black()
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
