\name{isopArea.assigner}
\alias{isopArea.assigner}
\docType{data}
\title{
Return Isoparalitoral Area Code from coordinates values.
}
\description{
These function takes a coordinates values from data (longitude/latitude), intersects them with a Isoparalitoral areas shape (old/new) and returns the corresponding code.

\code{old} parameter allows to choose which shape will be used: Ancient shape (1992, with errors) and New shape (2012).

Pairs of coordinates that falls out of 200mn range or with NA values for lon/lat will be listed as NA.
}
\usage{isopArea.assigner(data)}
\arguments{
  \item{data}{Data frame of points.}
  \item{colLon, colLat}{Positions of lon/lat columns. If no specified, the function will search headers "lon" and "lat", respectively.}
  \item{old}{Logical. Do you want to use "Old" shape of Isoparalitoral Areas?.}
}
\format{
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
}
\examples{
data <- data.frame(lon = c(runif(20, -80, -75), NA, 5, NA), lat = c(runif(20, -18, -15), 4, 8, NA))

isopArea.assigner(data)
}
\keyword{datasets}