#' Create a mask for areas that are unavailable for sampling
#'
#'
#' `aoi_createMask()` creates a mask using vector and raster layers (e.g. roads, streams, settlements, specific habitat types or custom polygons) that will be discounted from sampling site selection.
#'
#'
#'Any areas included in the (buffered) point, line or polygon files, as well as the supplied rasters will be combined into a single binary raster mask, representing the union of all layers. It will have the spatial extent, resolution and projecton of the layers in `rData`. All sf objects and rasters should be in the same projection, otherwise they will be reprojected to the projection of the raster data provided. Masks used in `sd_create_design` show areas to exclude (with 0=invalid; 1=valid).
#'
#'
#' @param vData Either a single sf object or a list of point, line or polygon sf objects that will optionally be buffered, and converted to a raster masked area.
#' @param rData Terra `SpatRaster` with required resolution, extent and projection with one or more raster layers. Any value above 0 or NA will be used as a mask. If no rasters are required for the mask, then at least one layer must be provided as a template, typically the result of `aoi_createAOI(method = "r")` function. If used as a template, the layers does not need to contain any values, but any NAs in this template will be added to the mask.
#' @param buffer Either a single number, representing a universal buffer (in projection units, e.g. metres) to be applied to all vector data in `vData` or a vector of numeric buffer values corresponding to the order of the data in `vData`.
#' @param maskValue Numeric. Value to use for masked area. Defaults to 1.
#' @param background Numeric. Value to use as background (unmasked area). Defaults to NA.
#' @return A terra `SpatRaster` with two values (or single value and NAs) representing area to mask.
#' @export
#'
#' @examples
#' \dontrun{
#' # How to specify buffers for each sf object:
#' vData <- list(points, lines, polygons)
#' buffer <- c(1000, 30, 0)
#' }

aoi_createMask <- function(vData, rData, buffer, maskValue = 1, background = NA){

  ## checks
  if(!inherits(rData, c("SpatRaster", "BasicRaster"))) stop("rData must be a SpatRaster or raster")

    ## do checks... convert to list and make sure are sf class
  if(!inherits(vData, "list")) vData <- list(vData)
  if(!all(sapply(vData, function(x) inherits(x, "sf")))){
    stop("vData must only contain sf objects")
    }
  
  # check background and maskvalues
  if(identical(maskValue, background)) stop("background and maskValue cannot be the same value!!!!")
  
  # Check and unify crs
  
  # get raster crs - single crs
  # crs.r <- sf::st_crs(rData)
  # get vector crs (may be different amont them as well as different to raster)
  # crs.list <- lapply(vData, st_crs)
  
  ## if problems with epsg version below, just swop crs.r and crs.list to above lines.
  
  # alternate - just check the EPSG code, sometimes has additional comments
  crs.r <- as.numeric(sub('.*ID\\[\\"EPSG\\",([[:digit:]]*)\\].*', "\\1", sf::st_crs(rData)$wkt))
  crs.list <- lapply(vData, function(x) {
    as.numeric(sub('.*ID\\[\\"EPSG\\",([[:digit:]]*)\\].*', "\\1", sf::st_crs(x)$wkt))
  })
  
  # check if vector CRS is same as raster crs
  indCRS <- sapply(crs.list, identical, y = crs.r)
  
    # if(length(unique(crs.list)) > 1)

  # transform all non matching vector crs to raster crs and update vData
  if(any(!indCRS)){
    vData.prj <- lapply(vData[!indCRS], sf::st_transform, crs = crs.r)
    vData[!indCRS] <- vData.prj
  }
  
  # check length of buffer is 1, or matches length of vData
  if(length(buffer) > 1) {
    
    if(!length(vData) == length(buffer)) stop("length of vData must match length of buffer")
    
  } 
  
  ## below runs into problem using different geometries to rasterise. Just need to rasterise each geometry type separately. And then join as rasters...
  
  # However, for now, just use a 0.1 buffer.and all will be polygons.. 
  
  # # check if buffer is 0 (eg for river).. need to index list, buffer those with - or + buffers, then regroup.
  # if(!all(length(buffer) == 1, identical(buffer, 0))){
  #   
  #   # could fix for now is to convert 0 buffer to minimal buffer.. eg 0.1
  #   
  #   # make index for 0s in buffer
  #   ind0 <- buffer > 0
  #   
  #   # if buffer is length one, then values are recycled in mapply
  #   vDataBuff <- mapply(function(x, y) sf::st_union(sf::st_buffer(sf::st_geometry(x), dist = y)),
  #                       vData[ind0], buffer[ind0], SIMPLIFY = FALSE)
  #   
  #   # update vData with buffered elements
  #   vData[ind0] <- vDataBuff
  #   
  #         }
  
  
  # check for 0 buffers and replace with 1m buffer. or could be 0.1
  ind0 <- buffer > 0
  buffer[!ind0] <- 0.1
  
  # if buffer is length one, then values are recycled in mapply
  vDataBuff <- mapply(function(x, y) sf::st_union(sf::st_buffer(sf::st_geometry(x), dist = y)),
                     vData, buffer, SIMPLIFY = FALSE)
  
  
  # convert from sfc to sf and rbind to single data frame
  all <- do.call(rbind, lapply(vDataBuff, function(x) st_sf(data.frame(geometry = st_geometry(x)))))
  # all; plot(all)
  
  # make spatvector collection
  # sptV.list <- lapply(vData, terra::vect)
  # svc <- terra::svc(sptV.list)
  # 
  sptV <- vect(all)
  
  # rasterise
  all.r <- terra::rasterize(sptV, rData, background = 0, touches = TRUE)
  #plot(all.r)
  
  # join together all rasters into single mask
  ## get NA from template raster, rData, and include any values from rData > 0
  rData.msk <- any(sum(c(is.na(rData), rData, all.r), na.rm = TRUE) > 0)
  #plot(rData.msk)
  
  ## do custom background, value, check background and maskValue are not 1 and 0.. 
  if(!all(identical(background, 0), identical(maskValue, 1))) {
    
    rcl <- matrix(c(1, maskValue, 0, background), ncol = 2, byrow = TRUE)
    rData.msk <- terra::classify(rData.msk, rcl = rcl)
  }
  
  return(rData.msk)
}


