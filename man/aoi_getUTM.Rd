% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aoi_createAOI.r
\name{aoi_getUTM}
\alias{aoi_getUTM}
\title{Get UTM zone for sf object in geographic coordinates}
\usage{
aoi_getUTM(x, project = TRUE, warn = TRUE)
}
\arguments{
\item{x}{sf object or path to shapefile or kml}

\item{project}{Logical. If \code{TRUE}, x will be returned in corresponding UTM projection}

\item{warn}{Logical. Output warnings.}
}
\value{
If project is \code{TRUE}, returns the projected sf object, if FALSE, returns a character vector of the EPSG code corresponding to an appropriate UTM zone for \code{x}.
}
\description{
\code{aoi_getUTM} will calculate an appropriate UTM zone for the centroid of your data, and will warn if the lower left corner or upper right corner fall in different UTM zones to the centroid. Optionally, it will project the inputs to the UTM zone.
}
