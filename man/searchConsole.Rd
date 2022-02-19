\name{searchConsole}
\alias{searchConsole}
\docType{data}
\title{
Search Console Data
}
\usage{data("searchConsole")}
\format{
  A data frame with 471 observations on the following 6 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{clicks}}{a numeric vector}
    \item{\code{impressions}}{a numeric vector}
    \item{\code{ctr}}{a numeric vector}
    \item{\code{position}}{a numeric vector}
    \item{\code{pagePath}}{a factor with levels \code{/entry/2015/01/17/064522} \code{/entry/2014/12/19/221230} \code{/entry/2014/03/05/001432} \code{/entry/2014/12/09/182525} \code{/entry/2015/01/15/073029} \code{/entry/2014/08/13/031519}}
  }
}
\examples{
data(searchConsole)
## maybe str(searchConsole) ; plot(searchConsole) ...
}
\keyword{datasets}
