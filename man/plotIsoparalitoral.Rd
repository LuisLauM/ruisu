\name{plotIsoparalitoral}
\alias{plotIsoparalitoral}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw Isoparalitoral Areas
}
\description{
Draw lines and polygons as specified by a Isoparalitoral areas shape.
}
\usage{
plotIsoparalitoral(database = "worldHires", xlim = NULL, ylim = NULL, add = FALSE, col.map = "black", bg = par("bg"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database}{
character string naming a geographical database, or a list of x, y, and names obtained from a previous call to map. See map function for further details.
}
  \item{xlim, ylim}{
numeric vectors of length 2, giving the x and y coordinates ranges.
}
  \item{add}{
logical flag that specifies whether to add to the current plot. If \code{FALSE}, a new plot is begun, and a new coordinate system is set up.
}
  \item{col.map}{
Color of the world map. This argument is ignored if \code{add} is \code{TRUE}.
}
  \item{bg}{
Background color.
}
  \item{bg}{
Background color.
}
  \item{areaList}{
Vector indicating Isoparalitoral areas code will be plotted.
}
  \item{areaCols}{
Vector indicating colors for selected Isoparalitoral areas. It is not necessary that the lengths are equal.
}
  \item{codeLabs}{
Do you want to show Codes in the graphic?
}
  \item{m.codeLab, cex.codeLab, offset.codeLab, col.codeLab}{
Arguments from the \code{textxy} function (calibrate package).
}
  \item{\dots}{
Extra arguments passed from graphics.
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
On the ellipsis, do not specify \code{col} argument, because \code{areaCols} replace this parameter.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
exampleAreas <- c(1050, 12033, 8140, 13123)

plotIsoparalitoral("worldHires", areaList = exampleAreas)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
