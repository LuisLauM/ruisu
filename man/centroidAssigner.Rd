\name{centroidAssigner}
\alias{centroidAssigner}
\docType{data}
\title{
Returns centroid values from Isoparalitoral-area codes.
}
\description{
The function takes a vector of isoparalitoral Areas and returns centroids, i.e. latitude and longitude values.

\code{old} parameter is used to indicate the type of shape used: Ancient (elaborated, roughly, in 1992; it has errors respect coast line) and New (made in 2012 from a shape of coastline more accurate). The variations between the values -about centroid- ranging from a few meters (for nearshore areas) to almost half a nautical mile (for those further away from the coast).
}
\usage{centroidAssigner(isoCode, old = TRUE)}

\arguments{
  \item{isoCode}{Vector of isoparalitoral-areas codes.}
  \item{old}{Logical. Do you want to use "Old" shape of Isoparalitoral Areas?}
}

\format{
  %%  ~~ Some format
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
areaCodes <- c(1050, 4043, 17073, 27103)

centroidAssigner(isoCode)
}
\keyword{datasets}
