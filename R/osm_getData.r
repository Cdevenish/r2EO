#' Extract layers from Open Street Maps
#'
#' Extract layers such as roads and settlements from OSM. OSM layers will be downloaded and cropped to the extent supplied in the function. OSM data can be filtered by specifying tags within osm features (e.g. cities within places) and/or by removing columns in the resulting data.
#'
#' @param x sf object from which an extent can be extracted. OSM data is downloaded within this bouding box.
#' @param key Character. OSM feature to download. e.g. highway for roads, place for settlements, waterway for rivers. See (https://wiki.openstreetmap.org/wiki/Map_Features) for details.
#' @param tags Character vector. Tags to subset an OSM feature. If missing, then default for key = "place" are c("city","town","village","hamlet","isolated_dwelling"), NULL for key = "highway", "river" for key = "waterway".
#' @param cols Character vector. Column names to subset result. Only these columns will be returned in osm extract. Must match column names in osm data. e.g. Recommended columns to keep:
#' highways: c("osm_id", "name", "highway", "surface")
#' places: c("osm_id", "name", "place")
#' @param dsn Character. Directory path to save osm data as shapefile. Will be saved in same projection as `x`. If NULL, then no file is saved.
#' @param suffix Character. Suffix to append to filename of shapefile.
#' @param ... Further arguments to `sf::st_write()`
#' @return If a path is supplied (`dsn`), then osm data will be saved. Invisibly returns an sf object of the osm data (e.g. points or lines).
#' @export

osm_getData <- function(x, 
                        key = c("highway", "place", "landuse", "waterway"), 
                        tags, cols, 
                        dsn=NULL, suffix = NULL, ...){

  # library(osmdata)
  # library(collapse)

  ## Create default tags
  
  # water / river tags.. check changes, add natural:water as well, and water:river? check lines and multilines

  if(missing(tags)){

    tags <- switch(key,

                   highway = NULL,
                   place = c("city","town","village","hamlet","isolated_dwelling"),
                   waterway = "river" 
                   )
  }
  # place:town		An important urban centre, between a village and a city in size.
  # place:village		A smaller distinct settlement, smaller than a town with few facilities available with people traveling to nearby towns to access these.
  # place	hamlet		A smaller rural community, typically with fewer than 100-200 inhabitants, and little infrastructure.
  # place	isolated_dwelling		The smallest kind of settlement (1-2 households).
  ## Also available....
  # place	locality		A named place that has no population.
  # place	farm		An individually named farm.

  ## see # https://wiki.openstreetmap.org/wiki/Map_Features
  # shell.exec("https://wiki.openstreetmap.org/wiki/Map_Features")


  crs <- sf::st_crs(x)

  # expand bbox, convert to wgs. (then convert back and clip at end)
  x_exp <- adjExt(x, d = 0.1, scale = 1.1, outF = "sf")
  x_wgs <- sf::st_transform(x_exp, crs = 4326)

  # build osm query
  q <- osmdata::opq(bbox = sf::st_bbox(x_wgs)) %>%
    osmdata::add_osm_feature(key = key, value = tags)

  # get data
  osm.env <- try(osmdata::osmdata_sf(q))

  if(!inherits(osm.env, "try-error")){

    osm <- switch(key,

                highway = osm.env$osm_lines,
                place = osm.env$osm_points,
                waterway = osm.env$osm_lines # check and combine with multilines and remove duplicates?
      )
  } else stop("OSM download error. Check server connection or try again later :)")


  #table(osm$place)
  #table(osm$highway)
  osm.proj <- sf::st_transform(osm, crs = crs)
  #plot(osm.proj[,"place"], pch = 16)

  ## clip to utm env (st_crop to crop to bounding box)
  osm.proj <- suppressWarnings(sf::st_intersection(osm.proj, x))


  if(!missing(cols)) {

    if(!all(cols %in% colnames((osm.proj)))) {
      stop("cols must only contain column names in osm table outputs.")
    }

    if (requireNamespace("collapse", quietly = TRUE)) {
      osm.proj <- collapse::ss(osm.proj, j = cols) # select columns
    } else {
      osm.proj <- subset(osm.proj, select = cols) # select columns
    }



  }

  if(!is.null(dsn)){

    sName <- sub("_\\.", "\\.", paste0(paste("osm", key, suffix, sep = "_"), ".shp"))
    sf::st_write(osm.proj, file.path(dsn, sName), delete_layer = TRUE, ...)

  }

  invisible(return(osm.proj))

}
