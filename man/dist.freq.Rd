\name{dist.freq}
\alias{dist.freq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate a plot array with length structure}
\description{From a frequency table size, generates a vertical array of graphics for each specified category (months, latitudes, years, etc.).}
\usage{
dist.freq(file, file2 = NA, profile = NULL, xlab = "Longitud (cm)", ylab = "Frecuencia (\%)", cex.axis = 1.2, xlim = NULL, Xinterval = 1, Yinterval = 10, lcol = "blue", lcol2 = NULL, lwd = 1, lty = "l", styleLab = 2, jValue = NA, jtext = NA, jcex = 1, cat.cex = 1, jround = 1, jcol = "red", jwd = 1, jty = "dotted", sinpesca = NA, sinjuveniles = NA, zeros = FALSE, nzeros = 0, showRange = TRUE, adj = 0.02, smooth = TRUE, spar = 0.01, totalFreq = FALSE, totalLabel = "Total")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{Character. File path of length frequencies table.}
  \item{file2}{Character. File path of second length frequencies table.}
  \item{profile}{Character. Profile by species.}
  \item{xlab}{A title for the X axis.}
  \item{ylab}{A title for the Y axis.}
  \item{cex.axis}{Size of axis text.}
  \item{xlim}{Limits for the X axis.}
  \item{Xinterval}{Intervals for the X axis.}
  \item{Yinterval}{Intervals for the Y axis.}
  \item{lcol}{Vector of colors for length lines (one or more).}
  \item{lcol2}{Color (just one) of length line for second table.}
  \item{lwd}{Width of length lines.}
  \item{lty}{Vector of line type for length lines (one or more).}
  \item{styleLab}{Numer (0,1,2,3), the style of axis labels.}
  \item{jValue}{Juvenile length.}
  \item{jtext}{String before juvenile percentage values.}
  \item{jcex}{Size of text for juveniles.}
  \item{cat.cex}{Size of categories text.}
  \item{jround}{Number of decimals in percentage of juveniles.}
  \item{jcol}{Color of juveniles line.}
  \item{jwd}{Width of juveniles line.}
  \item{jty}{Type of juveniles line.}
  \item{sinpesca}{Text if there is no catch.}
  \item{sinjuveniles}{Text if there was catch but there were no juveniles.}
  \item{zeros}{Logical. Plot zeros?.}
  \item{nzeros}{Numeric. Number of zeros to complete blanks.}
  \item{showRange}{Logical. Show range of lengths (max - mi).}
  \item{adj}{Adjust text inside plot.}
  \item{smooth}{Use spline method to smooth curve}
  \item{spar}{Factor of smooth.}
  \item{totalFreq}{Logical. If \code{TRUE}, function will show a curve (just one) with all grouped frequencies.}
  \item{totalLabel}{Label for the plot if \code{totalFreq = TRUE}.}
}
\details{
This function takes a length frequency data and returns an stacking plot of that frequencies.

\code{smooth}: logical. If \code{TRUE}, function uses an spline function to plot by categories.

This function uses a \code{data} with categories at cols and length at rows.
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Wencheng Lau
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Use interface at \emph{help} file.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
