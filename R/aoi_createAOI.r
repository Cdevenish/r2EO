## AOI function version
## Script for developing functions to create AOI

## Christian Devenish


## Aims
## Create area of interest for a project as a spatial object (polygon - rectangle) and transform to suitable projection
## AOI can be based on
## 1) existing sample points (eg Peru restoration project where intervetion areas are pre defined)
## 2) Client provided polygon (eg Anglo iNPI where mine concession area is initial aoi)
## and modified by
## including specific order stream networks (from hydroRivers), of set lengths, trajectories to coast, etc.
## buffer size (in distance around points, polygon)
## other provided spatial polygons/points to include

## Simplest aoi around existing points is convex hull + optional buffer

#' Create Area of Interest
#'
#' `aoi_createAOI` creates an area of interest from sample points or existing polygons and optionally, projects this to an appropriate UTM zone.
#'



#' @param x Character vector or sf object. If character, then path(s) (optionally relative to working directory) to shapefile or kml of spatial points or polygon(s). If multiple files are used, then the aoi is the union of all these files.
#' @param method Character. Method to create aoi. One of, ch (convex hull around points), env (rectangular envelope), r (raster template)
#' or river (tbc). Note `ch` and `env` may be extended by a `buffer` around them.
#' @param buffer Numeric scalar. Buffer distance (in metres) around points or polygon to create aoi, default is no buffer.
#' @param outF Character vector indicating output format for aoi. Can be one ore more of shp (shapefile), kml, rdata (sf object saved as .rdata)
#' @param res Numeric. Expected resolution of raster covariates (useful to have `env` and raster template aligned)
#' @param mask Logical. If TRUE, will create an additional raster masked outside the aoi
#' @param crs Force a particular projection on spatial input `x`, otherwise projected to corresponding UTM zone
#' @param dsn Character vector with filepath to folder to store output aoi. Either length one or equal to outF with a path for each format.
#' @return Returns an sf polygon object of the area of interest or a spatRaster object, depending on `method` requested (and optionally saves this to disk in requested format)
#' @export

aoi_createAOI <- function(x, method = c("ch", "env", "raster", "river"), buffer = 0, outF = c("shp", "kml", "tif", "rdata"), res = 1000, mask = FALSE, crs, dsn){


  # library is in dependencies. Remove from here
  # library(sf)

  # check args
  method <- match.arg(method)

  # check projection
  if(is.na(st_crs(x))) stop("x does not have associated projection information")

  # Get sf object, check geometry, validate
  sf1 <- getSF(x)
  # sf1 <- downEO:::getSF(x)
  
  # If crs is present, use this to project
  if(!missing(crs)) sf1 <- sf::st_transform(sf1, crs = crs) else {

    # otherwise find_utm and project
    sf1 <- aoi_getUTM(sf1, project = TRUE, warn = TRUE)
  }

  # get crs in standardised format (either new utm crs or will be same as crs input)
  crs <- sf::st_crs(sf1)

  ## standardise the extent bbox (and do raster)
  # get extent with buffer first, then adjust with neat origin and fitting resolution
  ## UPDATE TO terra::ext
  bbx <- sf::st_bbox(sf::st_buffer(sf::st_union(sf1), dist = buffer))
  bbx.adj <- adjExt(bbx, d = res, outF = "bbox")

  ## create aoi
  aoi <- switch(
    method,
    ch = sf::st_sf(geometry = sf::st_buffer(sf::st_convex_hull(sf::st_union(sf1)), dist = buffer)),
    env = sf::st_sf(geometry = sf::st_as_sfc(bbx.adj), crs = crs),
    raster = {
      # make template raster
      ext <- terra::ext(bbx.adj[c("xmin", "xmax", "ymin", "ymax")])
      r <- terra::rast(ext, crs = crs$wkt, res = res)
      names(r) <- "aoi.r"
      r[] <- 1 # otherwise can't stack below...

      # check
      # ext == raster::extent(r)

      ## make aoi mask
      if(mask) {
        aoi.msk <- r
        aoi.msk <- terra::mask(x = aoi.msk, mask = terra::vect(sf1))
        names(aoi.msk) <- "aoi.msk"
        r <- c(r, aoi.msk)
      }
      return(r)
    },
    river = stop("Not implemented yet")
  )


  # write results - only if dsn is present
  if(!missing(dsn)){

    outF <- match.arg(outF, several.ok = TRUE)

    # get name
    if(inherits(x, "character")) name <- sub("\\.[[:alnum:]]{3,4}$", "", basename(x)) else {
      if(inherits(x, "sf")) name <- deparse1(substitute(x))
    }

    for(out in outF){

      switch(out,

             shp = sf::st_write(aoi, file.path(dsn, paste0(name, ".shp"))),
             kml = sf::st_write(aoi, file.path(dsn, paste0(name, ".kml"))),
             tif = terra::writeRaster(aoi, file.path(dsn, paste0(name, ".tif"))),
             rdata = save(aoi, file = file.path(dsn, paste0(name, ".rdata")))
      )
    }
  } # end of write to disk

  return(aoi)

}


#' Get UTM zone for sf object in geographic coordinates
#'
#' `aoi_getUTM` will calculate an appropriate UTM zone for the centroid of your data, and will warn if the lower left corner or upper right corner fall in different UTM zones to the centroid. Optionally, it will project the inputs to the UTM zone.

#' @param x sf object or path to shapefile or kml
#' @param project Logical. If `TRUE`, x will be returned in corresponding UTM projection
#' @param warn Logical. Output warnings.
#' @export
#' @return If project is `TRUE`, returns the projected sf object, if FALSE, returns a character vector of the EPSG code corresponding to an appropriate UTM zone for `x`.


aoi_getUTM <- function(x, project = TRUE, warn = TRUE){


  #library(sf)

  if(!inherits(x, "sf")) sf1 <- downEO:::getSF(x) else sf1 <- x

  proj <- sf::st_is_longlat(sf1)

  if(is.na(proj)) stop("No projection information associated with sf object") else {

    # convert to geographic if not already
    if(!proj) {
      sf1 <- sf::st_transform(sf1, 4326)
      sf1 <- sf::st_make_valid(sf1) # not really needed...
    }
  }

  # get centroid
  centr <- sf::st_centroid(sf::st_union(sf1))

  # get longitude
  lon <- sf::st_coordinates(centr)[,1]
  lat <- sf::st_coordinates(centr)[,2]
  # The UTM system divides the surface of Earth between 80°S and 84°N latitude into 60 zones,
  # each 6° of longitude in width. Zone 1 covers longitude 180° to 174° W;
  # zone numbering increases eastward to zone 60 that covers longitude 174 to 180 East. [wikipedia]

  if(warn) if(lat < -80 | lat > 84) stop("No utm zone outside of 80°S and 84°N")

  ## get corners to check whether multiple zones are crossed
  bbox <- sf::st_bbox(sf1)

  lons <- unname(c(lon, bbox["xmin"], bbox["xmax"]))
  lats <- unname(c(lat, bbox["ymin"], bbox["ymax"]))

  # convert to UTM
  # https://stackoverflow.com/questions/9186496/determining-utm-zone-to-convert-from-longitude-latitude#9188972
  utmZ <- (floor((lons + 180)/6) %% 60) + 1
  utmNS <- ifelse(lats < 0, "S", "N")

  utm <- paste0(utmZ, utmNS)

  # check cross zone
  if(length(unique(utm)) != 1) {

    # get distance between min max (use global EA - ease grid 2.0 EPSG: 6933))
    bb <- sf::st_bbox(sf::st_transform(x, crs = "EPSG:6933"))
    xdist <- bb["xmax"] - bb["xmin"]; ydist <- bb["ymax"] - bb["ymin"]

    # x distance of 1 zone at lat is
    df1 <- sf::st_as_sf(data.frame(x = c(0, 6), y = rep(lat, 2)), coords= c("x", "y"), crs = 4326)
    dist1 <- as.numeric(sf::st_distance(sf::st_transform(df1, df1, crs = "EPSG:6933"))[1,2])

    # check whether spans equator
    eq <- length(unique(utmNS)) != 1

    if(eq) eWarn <- sprintf("\nAOI spans the equator, y distance: %1.0f km", ydist/1000) else eWarn <- NULL

    if(warn){
      warning("Multiple UTM zones. ", sprintf("Total x distance: %1.0f km", xdist/1000),
              sprintf("\n1 UTM zone at centroid latitude has x distance of %0.1fkm", dist1/1000),
              eWarn, "\nUsing centroid to define UTM zone",
              call. = FALSE)
    }

  }

  # Just use centroid UTM (if required, use the crs argument to force to regional projection)
  utmZ <- utmZ[1]
  utmNS <- utmNS[1]

  ## exceptions (Norway, Svalbard)
  if (lat > 55 & lat < 64 & lon > 2 & lon < 6){
    utmZ <- 32
  } else {
    if (lat > 71 & lon >= 6 & lon < 9){
      utmZ <- 31
    } else {
      if (lat > 71 & lon >= 9 & lon < 12){
        utmZ <- 33
      } else {
        if (lat > 71 & lon >= 18 & lon < 21){
          utmZ <- 33
        } else {
          if (lat > 71 & lon >= 21 & lon < 24){
            utmZ <- 35
          } else {
            if (lat > 71 & lon >= 30 & lon < 33){
              utmZ <- 35
            }}}}}}

  # update utm with exceptions and just centroid
  utm <- paste0(utmZ, utmNS)


  # get the EPSG code for a northern hemisphere UTM, add 32600 to its value. For southern hemisphere UTMs, add 32700.
  # for wgs 1984 datum
  epsg <- ifelse(utmNS == "N", utmZ + 32600, utmZ + 32700)

  print(paste("UTM zone:", utm))
  print(paste("EPSG:", epsg))

  if(project) return(sf::st_transform(sf1, crs = epsg)) else return(paste("EPSG:", epsg))

}
