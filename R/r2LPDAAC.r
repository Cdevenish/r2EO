#' r2EO: A package for downloading EO and GIS data
#'
#' Functions have 2 suffixes:
#'
#' aoi_ : to create study areas
#' eo_ : to download from LPDAAC
#' osm_: to download from Open Street Maps
#'
#' @section aoi_ functions:
#' aoi_* functions create basic study areas around polygons or points, with optional buffers and set up template rasters (standardising resolution, projection, extent, origin) for spatial covariates. An appropriate UTM zone is optionally used for all data.
#'
#' @section eo_ functions:
#' eo_* functions download digital elevation data from the apEEARS API. A free NASA Earth data account is required to access this service. see \url{https://urs.earthdata.nasa.gov/}: NASA Earthdata portal
#'
#'
#' @docType "_PACKAGE"
#' @name r2EO
