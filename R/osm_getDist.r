#' Create distance to Feature raster
#'
#' Creates distance rasters from osm feature data, e.g. roads, rivers or a
#' supplied feature data set (sf object).
#'
#' @param x SpatRaster. From aoi_getAOI() - provides extent, resolution and 
#' projection for resulting distance raster.
#' @param y sf object. Alternative source of feature (e.g. roads), will be used 
#' instead of osm data. If present, key, cols are ignored.
#' @param rasterize Logical. Rasterize before distance calculation. Increases 
#' speed of operation - see ?terra::distance()
#' @param outDir Character. Path to folder to save osm vector data (optional).
#' @param outDir2 Character. Path to folder to save resulting distance raster
#'  (optional).
#' @inheritParams osm_getData
#' @return SpatRaster with distance to feature (in m), invisibly.
#' @export


osm_getDist <- function(x, y, 
                        key, cols, 
                        outDir=NULL,
                        outDir2=NULL,
                        rasterize = TRUE){
  
  # if x is a raster, then
  bb <- st_as_sf(data.frame(geometry = st_as_sfc(st_bbox(x))))
  
  if(missing(y)){
    featDat <- osm_getData(bb,
                         key = key,
                         cols = cols,
                         dsn = outDir)
  } else {
    if(inherits(y, "sf")){
      featDat <- y
    } else stop("If y is present, then must be sf object.")
    
  }
  
  ## union to single feature
  featDat <- sf::st_union(featDat)
  
  dist <- terra::distance(x, 
                          vect(featDat), 
                          unit = "m",
                          rasterize = rasterize)
  
  if(!missing(outDir2)){
    
    if(missing(key)) {
      fName = "dist.tif"
      } else {
        fName <- paste0(key, "_dist.tif")
      }
    
    terra::writeRaster(dist, 
                       filename = file.path(outDir2, fName),
                       overwrite = TRUE)
  }
  
  return(dist)
}

