\name{getDepth}
\alias{getDepth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives depth value according a topographic data
}
\description{
Take lon/lat values from data and gives depth value according a topographic data.
}
\usage{
getDepth(data, colLon = "lon", colLat = "lat")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A \code{data.frame} that will be used to obtain longitude and latitude values.
}
  \item{colLon}{
\code{numeric}. Number of column for longitude. By default it tooks the value "lon".
}
  \item{colLat}{
\code{numeric}. Number of column for latitude. By default it tooks the value "lat".
}
  \item{depthDatafile}{
A \code{string} indicating the path of the nc file for bathymetry. If \code{NULL}, \code{getDepth} uses internal default data with \code{xlim = c(-100, -70)} and \code{ylim = c(-20, 0)}.
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
}