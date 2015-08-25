\name{getProportion}
\alias{getProportion}
\docType{data}
\title{
Generate proportional values from table.
}
\description{
From a table of numeric values, returns another table with proportional values.

\code{group} allows you to define whether the ratio will respect the values of each row (1), column (2) or full board (3).
}
\usage{getProportion(data)}
\arguments{
  \item{table}{Table that will be analyzed.}
  \item{group}{Proportion values will be generated from rows (1), columns (2) or whole data (3).}
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
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data <- with(airquality, table(OzHi = Ozone > 80, Month, useNA = "ifany"), 1)

getProportion(data, 1) # Grouping by row
getProportion(data, 2) # Grouping by column
}
\keyword{datasets}
