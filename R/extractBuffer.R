library(rgeos)

#' @import rgeos
#' @title Extract data from a buffer
#' @description This function extracts giver data from a buffer by performing an
#' intersection. A shapefile containing only the buffer can be provided in order to speed
#' up the process. Otherwise, a shapefile with the bounds from with to create the buffer
#' must be provided with the size of the buffer.
#' @param shpData Data to be extracted
#' @param buffer Shapefile with the buffer to extract the data from
#' @param shpBounds Shapefile from which the buffer will be created if a shapefile with
#' the buffer has not been provided.
#' @param width Width of the buffer
#'
#' @return A shapefile  contaning the intersection of the data and the buffer
#' @export
#'
#' @examples
extractBuffer <- function(shpData, buffer = NULL, shpBounds = NULL, width = 5000) {
  if (is.null(buffer)) {
    withBuffer <- gBuffer(shpBounds, width = width, byid = TRUE)
    buffer <- gDifference(withBuffer, shpBounds, byid = TRUE)
  }

  clipped <- gIntersection(shpData, buffer, byid = TRUE)
  clipped
}
