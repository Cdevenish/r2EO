#' Example river network
#'
#' A subset of a river network in the Amazon basin.
#'
#' @format ## `river`
#' A sf data frame with 233 rows and 4 columns:
#' \describe{
#'   \item{OBJECTID}{ID}
#'   \item{main}{Logical variable denoting whether river segment is a 'main' branch or not.}
#'   \item{year}{Categorical variable denoting seasonal status of river.}
#'   \item{length_km}{Length in kilometers or river segment}
#'   \item{geometry}{SF geometry column}
#' }
#' @source River network - internal use only.
"river"
