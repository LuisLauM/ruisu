\name{digitsum}
\alias{digitsum}
\docType{data}
\title{
Returns sum of digits of number.
}
\description{
This function returns sum of digits of a number.

If \code{recursive} is TRUE, consecutive sums are performed until to get a result of one digit.
}
\usage{digitsum(x)}
\arguments{
  \item{x}{Numeric vector.}
  \item{recursive}{Logical. Determine consecutive sums until to get a result of one digit.}
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
digitsum(523)

digitsum(c(12, 13, 23.4))
}
\keyword{datasets}
