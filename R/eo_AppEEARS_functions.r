#' API AppEEARS - Authentication
#'
#'
#' Authenticate API
#' Enter NASA EARTH EXPLORER username and password at prompt to access API
#' @return API token is returned (valid for 48 hours)
#' @export

eo_authAppEEARS <- function(){

  # in depends...
  # library(getPass)
  # library(httr)
  # library(jsonlite)

  # variables
  API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"

  # get user credentials
  user <- getPass::getPass(msg = "Enter NASA Earthdata Login Username: ")
  password <- getPass::getPass(msg = "Enter NASA Earthdata Login Password: ")

  # Encode the string of username and password
  secret <- jsonlite::base64_enc(paste(user, password, sep = ":"))

  # call login service, set the component of HTTP header, and post the request to the server
  response <- httr::POST(
    paste0(API_URL,"login"),
    httr::add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                      "Content-Type" ="application/x-www-form-urlencoded;charset=UTF-8"),
    body = "grant_type=client_credentials"
  )

  # Check content of the request
  if(response$status_code != 200) stop("Authentication not successful. Check username and password!")

  response_content <- httr::content(response)

  # Convert the response to the JSON object
  token_response <- jsonlite::toJSON(response_content, auto_unbox = TRUE)
  token <- paste("Bearer", jsonlite::fromJSON(token_response)$token)

  cat("Token will expire:", jsonlite::fromJSON(token_response)$expiration)

  # Clean up
  remove(user, password, secret, response)

  return(token)
}


#' LP DAAC Data Download
#' Download
#'
#'
#' @param sf filepath to spatial file (readable by sf package, eg .shp, or .kml) or sf object in R
#' @param product Data frame of products and layers (see `getProducts()` for options)
#' @param proj Character vector. Projection for requested data (see `getProjections()` for options), e.g. "native", or "geographic", "easegrid_2_global"
#' @param taskName Character vector. Name to identify task.
#' @param outFormat Character vector. File format for output. Either geotiff or netcdf4
#' @param startDate Start date for data series. If `recurring` is FALSE in format %m-%d-%Y e.g. '12-31-2020'; if recurring is TRUE, then in format %m-%d e.g. '12-31'
#' @param endDate End date for data series. See above for format.
#' @param recurring Logical. Are data to be downloaded for recurring time within start and end date. e.g. at monthly frequency. See below.
#' @param fromYear Only if recurring is TRUE. Start year for recurring series.
#' @param toYear Only if recurring is TRUE. End year for recurring series.
#' @param token Authentication token. Output of `authAppEEARS()`
#' @return json with task ID and status (can be used to query status later)
#' @export

eo_getAppEEARS <- function(sf, products, proj = "native", taskName, outFormat = c("geotiff","netcdf4"), startDate, endDate, recurring, fromYear, toYear, token, verbose = TRUE){

  API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"

  # read spatial file and get format (points or areas)
  # library(sf)
  # library(geojsonsf)
  # library(geojsonR)

  if(inherits(sf, "sf")) sp <- sf else {
    sp <- try({
      sp <- sf::st_read(sf, quiet = TRUE)
      sp
    })
    if(inherits(sp, "try-error")) stop("Check sf is proper path and can be read with st_read")
  }

  ## if sp isn't geographic, then convert and store original projection
  lonlat <- sf::st_is_longlat(sp) # either T/F or NA
  if(is.na(lonlat)) stop("x does not have associated projection information") else {
    # if projected then convert to wgs
    if(!lonlat){
      crs <- st_crs(sp) # this isn't used yet # TODO  or use it after download...
      sp <- sf::st_transform(sp, crs = 4326)
    }
  }

  # Check sf is a single geometry type
  if(length(unique(st_geometry_type(sp))) > 1) stop("sf must contain a single geometry type")

  # Type of task, it can be either "area" or "point"
  if((tp <- as.character(unique(st_geometry_type(sp)))) %in% c("POLYGON", "POINT", "MULTIPOINT", "MULTIPOLYGON")){

    taskType <- switch(tp,
           POLYGON = "area",
           POINT = "point",
           MULTIPOLYGON = "area",
           MULTIPOINT = "point"
    )

    rm(tp)

  } else stop("Geometry must be either point or polygon")

  # convert sf to geojson
  gjs <- try({
  gj <- geojsonsf::sf_geojson(sp, simplify = FALSE)
  # simplify FALSE ensures that FeatureCollection is made, even without fields
  gjs <- geojsonR::FROM_GeoJson(gj)
  gjs
  })
  if(inherits(gjs, "try-error")) stop("Error converting sf object to geojson")

  rm(sp)

  ## CHOOSE PROJECTION ####
  # check projection is one of allowed names [if not "native".. ??? todo]
  projNames <- eo_getProjection(silent = TRUE)
  proj <- match.arg(proj, choices = projNames$Name)

  ## Compile JSON object to submit to API ####

  # check taskName
  if(!all(is.character(taskName) & length(taskName) == 1)) stop("taskName must be length 1 character vector")

  # check outformat
  outFormat <- match.arg(outFormat)

  ## for recurring time series
  if(!is.logical(recurring)) stop("recurring must be logical")

  ## create nested dataframe

  # If you set the recurring to TRUE
  if(recurring){
    dates <- data.frame(startDate = startDate, endDate = endDate, recurring = recurring)
  dates$yearRange <- list(c(fromYear,toYear))
  } else {
  dates <- data.frame(startDate = startDate, endDate = endDate)
}
  out <- list(proj)
  names(out) <- c("projection")
  out$format$type <- outFormat

  # format the geojson
  gjs$features[[1]]$geometry$coordinates <- list(gjs$features[[1]]$geometry$coordinates)


  # Create a list of data frames
  task_info <- list(dates, products, out, gjs)
  names(task_info) <- c("dates", "layers", "output", "geo")       # Assign names
  task <- list(task_info, taskName, taskType)                     # Create a nested list
  names(task) <- c("params", "task_name", "task_type")            # Assign names

  # convert to json for submission
  task_json <- jsonlite::toJSON(task,auto_unbox = TRUE, digits = 10)

  ## submit the request #####
  # Post the request to the API task service
  response <- httr::POST(paste0(API_URL, "task"), body = task_json , encode = "json",
                 httr::add_headers(Authorization = token, "Content-Type" = "application/json"))

  # Retrieve content of the request
  task_content <- httr::content(response)
  task_response <- jsonlite::toJSON(task_content, auto_unbox = TRUE)

  # Print task status
  if(!verbose) cat("Task ID:", task_content$task_id, "\nStatus:", task_content$status)

  invisible(jsonlite::fromJSON(task_response))

}

#' Monitor the status of the task
#'
#'
#' Get the status of a previously submitted task
#' @param time Time (in seconds) for repeated checking of status. For single call, use NULL. Currently only checks repeatedly for most recent task.
#' @param n How many recent tasks to check status of?
#' @param task_id. Optional. Specify a particular task to check status of
#' @export

eo_getStatus <- function(time=NULL, n = 1, task_id, token){

  API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"

  if(!missing(task_id)) {

    task_id <- paste0("/", task_id)
    n <- 1

  } else task_id <- NULL

  # set n limit to 1 if time is not null
  if(!is.null(time)) n <- 1

  # Request the task status of last request from task URL (for multiple limits)
  stat_req <- httr::GET(paste0(API_URL,"task", task_id),
                    query = list(limit = n), httr::add_headers(Authorization = token))

  # Retrieve content of the request
  stat_response <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(stat_req), auto_unbox = TRUE))
  remove(stat_req)

  if(inherits(stat_response, "list")) {
    name <- stat_response$task_name
    status <- stat_response$status

    cat("\n", name, ":", status, "\n")

  } else {print(stat_response[, c("\ntask_name", "status")])}


  if(!is.null(time)) {

    if(!is.numeric(time)) stop("time must be numeric")

    # get status
    stat <- stat_response$status # will be list as limit == 1

    while (stat != 'done') {
      Sys.sleep(time)
      stat_req <- httr::GET(paste0(API_URL,"task", task_id), query = list(limit = 1),
                      httr::add_headers(Authorization = token))

      stat_response <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(stat_req), auto_unbox = TRUE))
      stat <- stat_response$status

      cat("\n", "...", stat)
    }
  }

  invisible(stat_response) # return this always
}


#' Data download
#'
#'
#' @param outDir Path to directory where data is downloaded
#' @param name Optional character string to append to downloaded files
#' @param limit Character vector of (partial) product names to limit downloaded files to
#' @return A vector of filepaths to the downloaded files or nothing if download is `FALSE`
#' @inheritParams eo_getStatus
#' @inheritParams eo_getAppEEARS
#' @export


eo_getData <- function(outDir, task_id, download = FALSE, name, limit = "ASTGTM_NC"){

  if(missing(name)) name <- NULL

  API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"
  # Data storage set up ####
  ## create directory
  if(!dir.exists(outDir)) dir.create(outDir)


  ## When task is done...
  #
  ## check how many files there are
  response <- httr::GET(paste0(API_URL, "bundle/", task_id), httr::add_headers(Authorization = token))
  bundle_response <- jsonlite::prettify(jsonlite::toJSON(httr::content(response), auto_unbox = TRUE))

  # call the bundle API and return all of the output files. Next, read the contents of the
  # bundle in JSON format and loop through file_id to automate downloading all of the output
  # # files into the output directory.

  bundle <- jsonlite::fromJSON(bundle_response)$files

  # subset downloads for just tifs or requested files
  if(!is.null(limit)){
    #file_types <- sub(".*\\.([[:alpha:]]{2,4}$)", "\\1", bundle$file_name)
    limitTF <- apply(sapply(limit, function(x) grepl(x, bundle$file_name)), 1, any)
    bundle <- bundle[limitTF,]
  }
  # Print files to download...
  print(paste(nrow(bundle), "file(s) to download. Total size: ", utils:::format.object_size(sum(bundle$file_size), "auto")))

  # Continue? TODO optional user ask here?

  if(download){

    # check and create... should be covered below as well... for subdirectories
    if(!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)

    # store filepaths to return
    filepaths_return <- vector(length = nrow(bundle))

    # id <- bundle$file_id[1]
    for (id in bundle$file_id){

      # print(paste("Downloading file", which(id == bundle$file_id)))
      # retrieve the filename from the file_id
      filename <- bundle[bundle$file_id == id,]$file_name


      if(is.null(name)){

        # if no suffix is needed, then just use original name
        filepath <- file.path(outDir, filename)

      } else {

        if(!is.null(limit)) {
          # if limited files are downloaded (eg just tifs) then simplify name (and remove additional folder)
          basename <- limit[sapply(limit, function(x) grepl(x, filename))]} else {

            # otherwise use whole name (remove extension here to enable addition of suffix below)
            basename <- sub("(.*)\\.[[:alpha:]]{2,4}$", "\\1", filename)

          }
        # get file extension (to add back on later)
        ext <- sub(".*\\.([[:alpha:]]{2,4}$)", "\\1", filename)
        filepath <- file.path(outDir, paste0(basename, "_", name, ".", ext))
      }

      # create a destination directory to store the file in (if needed, if not warning ignored)
      # this could go in is.null(name) if statement, but I supposed name could contain a directory?
      suppressWarnings(dir.create(dirname(filepath), recursive = TRUE))

      filepaths_return[] <- filepath

      # write the file to disk using the destination directory and file name
      response <- httr::GET(paste0(API_URL, "bundle/", task_id, "/", id),
                      httr::write_disk(filepath, overwrite = TRUE), httr::progress(),
                      httr::add_headers(Authorization = token))
    }

    return(invisible(filepaths_return)) # only returns if(download)

  } # end download if()
}

#' Get list of products and layers
#'
#'
#' Get dataframe of products available from API. see also https://lpdaac.usgs.gov/product_search/. Call without arguments to get the product dataframe. Then use product IDs from the 'ProductAndVersion' column to get specific layers. The resulting data frame can be subsetted (if necessary) and used as input to `getAppEEARS()`
#' @param products Character vector with 'ProductAndVersion' of required layer. e.g. for Aster DEM v003: "ASTGTM_NC.003" Names taken from product list obtained from `getProduct()` called without arguments.
#' @return If products is NULL, a dataframe of all available products and associated information, if specific products are requested, then a dataframe of product and layer names
#' @export
#'
#' @examples search for specific products in dataframe produced from this function.
#' prod.df <- getProduct()
#' prod.df[grepl("dem", prod.df$Description, ignore.case = T),]
#' prod.df[grepl("AST", prod.df$Product, ignore.case = T),]


eo_getProduct <- function(products = NULL){

  API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"
  ## Get products ####
  # Request the info of all products from product service
  prods_req <- httr::GET(paste0(API_URL, "product"))

  # Retrieve the content of request
  all_Prods <- jsonlite::toJSON(httr::content(prods_req), auto_unbox = TRUE)
  prod.df <- jsonlite::fromJSON(all_Prods)
  rm(prods_req, all_Prods)

  if(!is.null(products)){

    # match arguments will possibilities
    products <- match.arg(products, choices = prod.df$ProductAndVersion, several.ok = TRUE)

    # show product layers
    layerList <- lapply(products, function(x) {

      names <- try({
        # Request the info of a product from product URL
        req <- httr::GET(paste0(API_URL,"product/", x))

        # Retrieve content of the request
        content <- httr::content(req)

        # Convert the content to JSON object
        response <- jsonlite::toJSON(content, auto_unbox = TRUE)

        # Remove the variables that are not needed anymore
        remove(req, content)

        # get layer names
        names(jsonlite::fromJSON(response))
      })

      data.frame(product = rep(x, length(names)), layer = names)

    }
    )

    layer.df <- do.call(rbind, layerList)


    return(layer.df)

  } else return(prod.df)
}


#' Get projection info
#'
#'
#' Get available projection information corresponding to products and platforms
#' @param silent Logical. Print projection information
#' @returns A data frame of names, descriptions and corresponding platforms for where projections are available (invisibly). Optionally prints the data frame to the console.
#' @export

eo_getProjection <- function(silent = FALSE){

  API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/"
  # Request the projection info from API_URL
  projs <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(httr::GET(paste0(API_URL, "spatial/proj"))), auto_unbox = TRUE))

  if(!silent) {
    print(projs[projs$Available, c("Name", "Description", "Platforms", "EPSG", "Units", "GridMapping")])
  }

  invisible(projs)
}


#' Delete a task
#' @inheritParams eo_getStatus
#' @inheritParams eo_getAppEEARS
#' @export

eo_deleteTask <- function(task_id, token){

  API_URL <- "https://appeears.earthdatacloud.nasa.gov/api/task/"
  response <- httr::DELETE(paste(API_URL, task_id, sep = ""),
                           httr::add_headers(Authorization = token))

  if(response$status_code == 204) cat("Task successfully deleted") else cat("Check status")
  invisible(response)
}
