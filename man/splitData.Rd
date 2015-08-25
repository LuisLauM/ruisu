\name{splitData}
\alias{splitData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split data in sections by date criteria.
}
\description{
These function takes a data table (from csv file) and two vectors of initial and final dates (format DD/MM/AAAA) and split data by them. If  these arguments are not specificated, function will requested them.
}
\usage{
splitData(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Character. File path of data will be splited. Required camps (column names): year, month, day.
}
  \item{iniDates}{
Initial dates. Format DD/MM/AAAA.
}
  \item{finDates}{
Final dates. Format DD/MM/AAAA.
}
  \item{listMonths}{
Vector of months to consider each year.
}
  \item{save}{
Logical. Do you want to save the final table? If FALSE, final table will be returned as data frame.
}
  \item{filename}{
Character. If \code{save} is TRUE, name of final data file.
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
file <- "data/anchosur.csv"

splitData(file)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
