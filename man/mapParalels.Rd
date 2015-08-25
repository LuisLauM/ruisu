\name{mapParalels}
\alias{mapParalels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw typical plots of paralels coast lines
}
\description{
These function takes a table (from .csv file) with coordinates (lon/lat) and a categories column, generates plots with paralels coast lines and plot corresponding points.
}
\usage{
mapParalels(file, catCol, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Character. File path of length frequencies table.
}
  \item{catCol}{
Column contains categories to split coordinates.
}
  \item{number.cat}{
Theorical number of categories. Values in \code{catCol} must be included on it.
}
  \item{names.cat}{
Vector indicating names of categories. Lengths between this argument and \code{number.cat} must be the same.
}
  \item{namespace.cat}{
This argument allows to adjust names inside the plot.
}
  \item{pos.cat}{
\code{las} argument (review \code{par} arguments).
}
  \item{adj.catY}{
Vertical adjust forcategories names.
}
  \item{adj.catX}{
Horizontal adjust forcategories names.
}
  \item{abr.cat}{
Do you want to abbreviate categories names? (Three first letters)
}
  \item{adj}{
Internal adjust for coast lines.
}
  \item{adjLon}{
Extra space (in degrees) for X axis.
}
  \item{lwd.box}{
width for external border.
}
  \item{oma}{
External space of graphic.
}
  \item{ylim}{
Limits for Y (latitude).
}
  \item{cex.axisY}{
Text size for X label.
}
  \item{atY}{
Interval for Y axis.
}
  \item{xlim}{
Limits for X (longitude).
}
  \item{interior}{
If \code{FALSE}, interior segments are not drawn. An interior segment is a line segment of the map that bounds two of the polygons to be drawn.
}
  \item{col.map}{
Color of land.
}
  \item{col.points}{
Vector of color of points.
}
  \item{lwd.map}{
Width of of map line.
}
  \item{hires}{
Resolution of map line.
}
  \item{offset}{
controls the distance between the label and the point. A value of 0 will plot labels on top of the point. Larger values give larger separation between point and label.
}
  \item{allHarbors}{
Do you want to show name of harbors in all maps?
}
  \item{pch.harbors}{
Either an integer specifying a symbol or a single character to be used as the default in plotting harbors points. 
}
  \item{cex.harbors}{
Size of harbor's point character.
}
  \item{tcex.harbors}{
Size of harbor's text.
}
  \item{alpha}{
Transparency of points.
}
  \item{pch.points}{
Either an integer specifying a symbol or a single character to be used as the default in plotting points. 
}
  \item{cex.points}{
Size of points.
}
  \item{col.cat}{
Text color for categories.
}
  \item{allHarborPoints}{
Do you want to show harbors points in all maps?
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
See interface script \code{calculateParalels}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data <- read.csv("data")

mapParalels(file, 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
