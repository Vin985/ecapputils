library(FRutils)
library(geosphere)
library(rgdal)
library(sp)

#' @export
#'@title Create a grid using latitude and longitude values.
#'
#'
#'@description This function creates a \code{\link{SpatialPolygonsDataFrame}} grid using the boundaries and cell size selected by the user.
#'@param x  A \code{\link{data.frame}} containing observations.
#'@param Latitude A pair of coordinates that identifies the grid northern and southern limits.
#'@param Longitude A pair of coordinates that identifies the grid eastern and western limits.
#'@param Grid.size size of the grid cells in km.
#'@param Clip Should the grid be clipped with a shapefile (\env{TRUE}) or not (\env{FALSE}).
#'@param clip.shape Shapefile to use to clip the grid if Clip is set to \env{TRUE}
#'@param projection coordinate reference system ID for the grid.
#'@details  The projection used for the grid must match the projection used for the dataset if you want to use the grid for \code{\link{effort.grid}} and \code{\link{species.grid}}.
#'@section Author:Christian Roy
#'@section note:Large grid with small cell size can take a while to create. Be patient.
#'@examples
###Create a grid between laitude 45N and 54N, and longitude 56W and 70W.
#'new.grid<-create.grid(Latitude=c(45, 54), Longitude=c(-70, -56), Grid.size=c(25000, 25000), Clip=FALSE, clip.shape=canshp, projection=NULL)
#'#plot the grid
#'plot(new.grid, axes=T)
#'#END

create.grid <-
  function(Latitude = c(45, 52),
           Longitude = c(-70, -56),
           Grid.size = c(0.1, 0.1),
           Clip = FALSE,
           clip.shape,
           hexgrid = FALSE,
           hexproj = "+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",
           projection = NULL) {
    ####Define a projection if no projection was defined previously
    if (is.null(projection) == T) {
      projection <-
        " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    }

    #Fail checks
    if (length(Latitude) != 2)
      stop("You need a pair of coordinates in Latitude for the grid")
    if (length(Longitude) != 2)
      stop("You need a pair of coordinates in Longitude for the grid")
    if (length(Grid.size) > 2)
      stop("There are only two dimensions in a grid")
    if (Clip == T) {
      if (class(clip.shape)[1] != "SpatialPolygonsDataFrame")
        stop("Clip.shape must be a shapefile")
      if (projection != proj4string(clip.shape))
        stop("Clip.shape and grid must be in the same projection")
    } else{
      if (class(Clip) != "logical")
        print("Clip was set to FALSE therefore the grid was not clipped")
    }


    #Make sure there are spacing for x and y
    if (length(Grid.size) == 1)
      Grid.size = c(Grid.size, Grid.size)

    #Transform meters in degrees
    long.dist1 <-
      abs(matrix(c(Longitude[1], Latitude[1]), ncol = 2) - geosphere::destPoint(
        c(Longitude[1], Latitude[1]),
        b = 90,
        d = Grid.size[1],
        r = 6378137
      ))[1]
    long.dist2 <-
      abs(matrix(c(Longitude[2], Latitude[1]), ncol = 2) - geosphere::destPoint(
        c(Longitude[2], Latitude[1]),
        b = 270,
        d = Grid.size[1],
        r = 6378137
      ))[1]
    lat.dist1 <-
      abs(matrix(c(Longitude[1], Latitude[1]), ncol = 2) - geosphere::destPoint(
        c(Longitude[1], Latitude[1]),
        b = 180,
        d = Grid.size[2],
        r = 6378137
      ))[2]
    lat.dist2 <-
      abs(matrix(c(Longitude[2], Latitude[1]), ncol = 2) - geosphere::destPoint(
        c(Longitude[2], Latitude[1]),
        b = 0,
        d = Grid.size[2],
        r = 6378137
      ))[2]
    degree.dist <-
      c(mean(c(long.dist1, long.dist2)), mean(c(lat.dist1, lat.dist2)))

    #Create the grid centroids
    xseq <- seq(Longitude[1], Longitude[2], by = degree.dist[1])
    yseq <- seq(Latitude[1], Latitude[2], by = degree.dist[2])
    grid.pts <- expand.grid(xseq, yseq)
    names(grid.pts) <- c("x", "y")

    #Create the spatial references
    coordinates(grid.pts) <-  ~ x + y
    proj4string(grid.pts) <- projection
    ##Create the grid


    if (hexgrid) {
      print("creating hexagonal grid")
      grid.pts <- spTransform(grid.pts, CRS(hexproj))
      grid.shp <- FRutils::hexgrid(grid.pts, convex = TRUE, width = 50000, seed = 111)
      names(grid.shp) <- "ID"
      grid.shp <- spTransform(grid.shp, projection)
    } else {
      grid.shp <- as.SpatialPolygons.GridTopology(points2grid(grid.pts))
      grid.shp <-
        SpatialPolygonsDataFrame(grid.shp, data = data.frame(ID = as.integer(gsub(
          "g", "", names(grid.shp)
        )),
        row.names =
          row.names(grid.shp)))
      proj4string(grid.shp) <- projection
    }

    if (Clip == T) {
      to.remove <- gContains(clip.shape, grid.shp, byid = T)
      if (any(to.remove == T) == F) {
        stop("Clip.shape are not superposed")
      }
      remove.vector <-
        sort(unlist(lapply(1:length(clip.shape@polygons), function(i) {
          which(to.remove[, i] == T)
        })))
      grid.shp <-
        grid.shp[grid.shp$ID %in% grid.shp$ID[-remove.vector], ]
    }

    grid.shp@data$CELL.NUM <- seq(1, length(grid.shp), by = 1)
    return(grid.shp)
    #END of the function
  }


extractBuffer <- function(shpData, buffer = NULL, shpBounds = NULL, width = 5000) {
  if (is.null(buffer)) {
    withBuffer <- gBuffer(shpBounds, width = width, byid = TRUE)
    buffer <- gDifference(withBuffer, shpBounds, byid = TRUE)
  }

  clipped <- gIntersection(shpData, buffer, byid = TRUE)
  clipped
}
