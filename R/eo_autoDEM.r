#' Automated one step download of DEMs from LPDAAC
#'
#' Download a digital elevation product (Aster or SRTM) from LPDAAC cropped to given extent, and optionally, transform to a utm projection, and create a hillshade raster. Other products (see `eo_getProducts()`) are also possible to download and project with this function.
#'
#' *Note* The function will wait until the LPDAAC task has completed. If this is taking a long time, cancel and run `eo_getStatus()` without arguments to see the status of the most recent task. When this shows as 'done', the files can be downloaded with `eo_getData()`.
#'
#'
#' @param products Character vector. Product names, can be shortened to 'ASTER' and/or 'SRTM', or full ProductandVersion names (see products in `eo_getAppEEARS()`)
#' @param outDir2 Alternative output folder for projected and hillshade rasters. Defaults to `outDir`
#' @param projTo  SpatRaster template for projection, as output by `aoi_createRaster()` - containing, desired, extent, resolution and projection information; or coordinate reference information. See `?terra::project()` for more details. Coordinate reference formats: WKT, PROJ.4 (e.g., +proj=longlat +datum=WGS84), or an EPSG code (e.g., "epsg:4326").
#' @param hillshade Logical. If `TRUE`, hillshade is also created from projected raster. Further arguments to `terra::shade()` may be included in ...
#' @param tifOnly Logical. If `TRUE`, only principal tif files will be downloaded for each product.
#' @param write Logical. Should processed rasters be written to disk.
#' @param out Character vector. Output format. Either 'fp' to return file paths of downloaded rasters; 'wgs' to return only rasters in native projection; 'proj' to return projected (and hillshade) rasters (only if projTo is included)
#' @param ... Further argumens to `terra::shade()`
#' @inheritParams eo_getAppEEARS
#' @inheritParams eo_getData
#' @return Either a character vector of filenames written to disk, or a SpatRaster object containing either the downloaded rasters in native projection, or the processed rasters
#' @seealso \code{\link{eo_getProduct}} to search all downloadable layers, \code{\link{eo_getAppEEARS }} to request a data download from the AppEAEARS API, \code{\link{eo_getStatus}} to query the status of a data request.
#' @export

eo_autoDEM <- function(sf, products = c("ASTER", "SRTM"), taskName, token, outDir, outDir2, projTo, hillshade = TRUE, tifOnly = TRUE, write = TRUE, out = c("proj", "fp", "wgs"), ...) {

  # need libary(terra)

  # get sf object from object or path
  sf1 <- getSF(sf)

  out <- match.arg(out)
  if(out == "proj" & missing(projTo)) stop("Must include projTo if projected rasters are required as output")
  
  if(!missing(projTo) & !exists(projTo)) stop("projTo is not found")

  # products can be a data frame with product and layers, or vector
  if(!inherits(products, "data.frame")){
    # replace shortnames
    products <- replace(products, na.omit(match(c("ASTER"), products)), values = "ASTGTM_NC.003")
    products <- replace(products, na.omit(match(c("SRTM"), products)), values = "SRTMGL1_NC.003")

    # get layer names
    layer.df <- eo_getProduct(product = products)

  } else layer.df <- products

  if(tifOnly) limit <- layer.df$product else limit <- NULL

  ## set up remaining parameters for API call
  proj <- "native"
  if(missing(taskName)) taskName <- "task1"
  startDate <- '01-01-2000' # doesn't matter for dems..
  endDate <- '03-01-2000'
  # SRTM  2000-02-11        2000-02-21
  # ASTER 2000-03-01        2000-03-01

  recurring <- FALSE
  if(missing(outDir)) outDir <- getwd()
  if(missing(outDir2)) outDir2 <- outDir


  # send Call
  taskID <- eo_getAppEEARS(sf=sf1, products=layer.df, proj=proj, taskName=taskName,
                        startDate=startDate, endDate=endDate,
                        recurring = FALSE, token = token, verbose = FALSE)

  ## check status until done, and then download
  status <- eo_getStatus(time=30, token = token, task_id = taskID$task_id)

  ## Download - should be done, from above.
  if(status$status == "done"){

    fp <- try({
    eo_getData(outDir= outDir, task_id = taskID$task_id, download = T, limit = limit, name = taskName)

  })
  } else stop("Check task status with eo_getStatus() with no arguments for last task")

  ## project raster (if crs is present)
  if(!missing(projTo)) {

      if(inherits(fp, "try-error")) {

    stop("Download error. Try runnning eo_getData() manually")
        } else {

          #find rasters
    tifs <- list.files(path = file.path(outDir), pattern = ".*\\.tif$", recursive = TRUE, full.names = TRUE)

    # subset for our downloaded rasters from what is actually on disk
    tifs <- tifs[basename(tifs) %in% basename(fp)]

        }

    library(terra) # seems that an internal function is not recognised in terra::terrain??

  # import as rasters
  r.native <- terra::rast(tifs) # as multilayer raster if length(tifs) > 1

  if(write) {

    # check outDir2 exists, and create, if necessary
    if(!dir.exists(outDir2)) dir.create(outDir2, recursive = TRUE)

    filename <- file.path(outDir2, sub("\\.tif$", paste("_proj.tif"), basename(tifs)))
    fp <- c(fp, filename) # update fp to return

    } else filename <- ""

  # do projection to parameters in projTo
  r.utm <- terra::project(r.native, projTo, method = "bilinear", filename = filename, overwrite = TRUE)

  # do hillshade
  if(hillshade){
    ## do hillshade

    if(write) {
      filename <- file.path(outDir2, sub("\\.tif$", paste0("_hllshd", ".tif"), basename(tifs)))
      fp <- c(fp, filename)
    } else filename <- ""

    r.terr <- terra::terrain(r.utm, v = c("slope", "aspect"), unit = "radians")
    r.hshd <- terra::shade(r.terr$slope, r.terr$aspect, normalize = TRUE,
                           filename = filename, overwrite = TRUE, ...)
    # additional arguments to hillshade in ...

    # stack for return
    r.utm <- c(r.utm, r.hshd)

    } #end of hillshade if()
  } # end of project/hillshade.

  # Return filepaths either to downloaded files from eo_getData or to projected and/or hillshaded tifs
  cat("These files written to disk:\n", paste(fp, collapse = "\n"), "\n")

  # return
  switch(out,
         fp = return(invisible(fp)),
         wgs = return(r.wgs),
         proj = return(r.utm)
         )
}

