% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotIsoparalitoral.R
\name{plotIsoparalitoral}
\alias{plotIsoparalitoral}
\title{Plot Isoparalitoral shapes}
\usage{
plotIsoparalitoral(
  codeList = NULL,
  add = FALSE,
  old = TRUE,
  plotParams = list(xlim = c(-90, -70), ylim = c(-18, -2), axes = TRUE, xlab = NA, ylab =
    NA),
  mapParams = list(database = "world"),
  ...
)
}
\arguments{
\item{codeList}{AIP codes to plot.}

\item{add}{logical flag that specifies whether to add to the current plot. If FALSE, a new plot is begun, using
coordinates for Peruvian shore.}

\item{old}{\code{logical}. Specifying whether to use old AIP shape (\code{AIPShapefile_old}) o the new (\code{AIPShapefile_new}).}

\item{plotParams}{Extra parameters passed to \code{\link{plot}} function. This argument is ignored if \code{add = TRUE}.}

\item{mapParams}{Extra parameters passed to \code{\link{map}} function. This argument is ignored if \code{add = TRUE}.}

\item{...}{Extra parameters passed to \code{plot.SpatialPolygons} function.}
}
\description{
Plot Isoparalitoral shapes
}
\details{
\code{plot.SpatialPolygons} is an internal function of \code{\link{sp}}. Some important parameters are: \code{border} which
allows to specify color (or colors if a vector) of each AIP. \code{col} is useful to modify fill color of AIP.
}
