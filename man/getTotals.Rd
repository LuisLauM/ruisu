\name{getTotals}
\alias{getTotals}
\docType{data}
\title{
Add columns and rows with Total values.
}
\description{
From a table of numeric values, returns another table with extra row and column of Total values.
}
\usage{getTotals(data)}
\arguments{
  \item{table}{Table that will be analyzed.}
  \item{group}{Total values will be generated for rows (1), columns (2) or both (3).}
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

getTotals(data, 1) # Grouping by row
getTotals(data, 2) # Grouping by column
}
\keyword{datasets}
