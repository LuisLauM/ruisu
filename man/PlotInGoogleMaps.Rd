\name{PlotInGoogleMaps}
\alias{PlotInGoogleMaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot coordinate points in a GoogleMaps layer.
}
\description{
Generates a ggplot object of points with a GoogleMaps layer behind.

Since it is a ggplot object, extra arguments can be added by latter by \code{+} operator.
}
\usage{
PlotInGoogleMaps(data, colLon = NA, colLat = NA, zoom = 5, size = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data of coordinates.
}
  \item{colLon, colLat}{
Number of column for coordinates (lon/lat).
}
  \item{zoom}{
map zoom, an integer from 3 (continent) to 21 (building), default value 10 (city). 
}
  \item{...}{
Extra arguments passed to \code{geom_point} funtion.
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
data <- data.frame(lon = runif(20, -80, -70), lat = runif(10, -18, -3))

PlotInGoogleMaps(data, size = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
