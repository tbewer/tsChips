#' @title Plot TS Chips
#' 
#' @description Plot image TS chips based on a location, area buffer, and time buffer
#' 
#' @param x RasterBrick. Image time series brick
#' @param loc Location. Can be a vector of length 2 representing the x,y coordinates, or a SpatialPolygons object of \code{nrow = 1} (the first row will be taken if \code{nrow(loc) > 1}), or an extent object (which will be extended if a buffer > 0 is given; see below)
#' @param buff Numeric. Number of pixels to buffer the location in all directions. A higher buffer will essentially zoom out.
#' @param start Date. OptionaL: earliest date ("yyyy-dd-mm") to display.
#' @param end Date. Optional: latest date ("yyyy-dd-mm") to display.
#' @param percNA Numeric. Maximum allowable \% NA in the cropped image chips
#' @param cols Character. Name of colour map to use (see display.brewer.all()) or a character vector with two or more colour names or hexadecimal values (as strings) between which to interpolate.
#' @param nbks Numeric. Number of breaks in the colour map
#' @param nc/nr Numeric. Number of columns and rows to plot, respectively. If the number of layers is greater than \code{nc*nr}, a screen prompt will lead to the next series of plots. These cannot exceed 4.
#' @param ggplot Logical. Produce a ggplot time series plot object?
#' 
#' @return \code{NULL} if \code{ggplot = FALSE} or an object of class \code{ggplot} if \code{ggplot = TRUE}, with the side effect of time series chips being plotted in both cases.
#' 
#' @author Ben DeVries
#' 
#' @import bfastSpatial
#' @import RColorBrewer
#' @import ggplot2
#' @import rgeos
#' @export
#' 
#' @references
#' Cohen, W. B., Yang, Z., Kennedy, R. (2010). Detecting trends in forest disturbance and recovery using yearly Landsat time series: 2. TimeSync - Tools for calibration and validation. Remote Sensing of Environment, 114(12), 2911–2924.
#' 
#' @examples
#' \dontrun{
#' library(bfastSpatial)
#' data(tura)
#' 
#' tsChips(tura, loc = c(820796, 831198))
#' tsChips(tura, loc = c(820796, 831198), buff = 50) # zoom out
#' tsChips(tura, loc = c(820796, 831198), buff = 50, percNA = 80) # allow more NA's in field of view
#' tsChips(tura, loc = c(820796, 831198), start = "2007-01-01", end = "2012-01-01", ggplot = TRUE) # restrict dates and produce pixel time series plot afterwards
#' }


tsChips <- function(x, loc, start = NULL, end = NULL, buff = 17, percNA = 20, cols = "PiYG", nbks = 35, nc = 3, nr = 3, ggplot = FALSE) {
  
  # get sceneinfo
  s <- getSceneinfo(names(x))
  
  # reformat buffer using image resolution
  buff <- buff * res(x)[1]
  
  # check location format and make a buffered extent object
  if(class(loc) == "numeric" & length(loc) != 2){
    stop("loc should be either a numeric vector of length 2 or a spatial object (polygon or extent).")
  } else if(class(loc) == "numeric"){
    e <- extent(c(loc[1] - buff, loc[1] + buff, loc[2] - buff, loc[2] + buff))
  } else if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
    if(nrow(loc) > 1){
      warning("only taking the 1st polygon of loc")
      loc <- loc[1, ]
    }
    e <- extent(loc)
    e <- extent(c(xmin(e) - buff, xmax(e) + buff, ymin(e) - buff, ymax(e) + buff))
  } else if(class(loc) == "extent"){
    e <- loc
    e <- extent(c(xmin(e) - buff, xmax(e) + buff, ymin(e) - buff, ymax(e) + buff))
  }
  
  # crop input brick
  xe <- crop(x, e)
  se <- getSceneinfo(names(xe))

  # start and end dates
  if(!is.null(start)){
    start <- as.Date(start)
    xe <- subset(xe, subset = which(se$date >= start))
    se <- getSceneinfo(names(xe))
  } else {
    start <- as.Date(min(se$date)) # to be used in ggplot later
  }
  
  if(!is.null(end)){
    end <- as.Date(end)
    xe <- subset(xe, subset = which(se$date <= end))
    se <- getSceneinfo(names(xe))
  } else {
    end <- as.Date(max(se$date)) # to be used in ggplot later
  }
  
  # reorder scenes
  xe <- subset(xe, subset = order(se$date))
  se <- getSceneinfo(names(xe))
  
  # filter out scenes with too many NA's
  if(percNA > 100)
    percNA <- 100
  nas <- sapply(freq(xe), FUN=function(x) as.numeric(x[is.na(x[, 1]), 2] / ncell(xe) * 100))
  nas[which(sapply(nas, length) == 0)] <- 0
  nas <- unlist(nas)
  if(percNA == 0){
    xe <- subset(xe, subset = which(nas == percNA))
  } else {
    xe <- subset(xe, subset = which(nas < percNA))
  }
  
  # final sceneinfo data.frame
  se <- getSceneinfo(names(xe))
  
  # colour map
  if(length(cols) == 1){
    require(RColorBrewer)
    cols <- colorRampPalette(brewer.pal(9, cols))(nbks)
  } else {
    cols <- colorRampPalette(cols)(nbks)
  }
  breaks <- seq(minValue(min(xe)), maxValue(max(xe)), length = nbks)
  
  # plots on separate screens if needed
  if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
    addfun <- function() plot(loc, add=TRUE)
  } else {
    addfun <- function() NULL
  }
  op <- par(mfrow = c(nr, nc))
  pps <- nc * nr
  nscreens <- ceiling(nlayers(xe) / pps)
  for(i in seq(1, nlayers(xe), by = pps)){
    if((nlayers(xe) - i) < pps){
      xes <- subset(xe, subset = c(i:nlayers(xe)))
      par(op)
      plot(xes, breaks = breaks, col = cols, main = getSceneinfo(names(xes))$date, legend=FALSE, nc = nc, nr = nr, addfun = addfun)
    } else {
      xes <- subset(xe, subset = c(i:(i + pps - 1)))
      plot(xes, breaks = breaks, col = cols, main = getSceneinfo(names(xes))$date, legend=FALSE, nc = nc, nr = nr, addfun = addfun)
      readline("Press any key to continue to next screen: \n")
    }
  }
  
  # TODO:
  # 1. add option to plot one or more RE scenes at end of ts if they are available.
  # 2. make a ggplot time series plot and plot it on final screen and/or output from function
  
  if(ggplot){
    require(ggplot2)
    if(is.numeric(loc)){
      s$response <- x[cellFromXY(x, loc)][1, ]
    } else if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
      require(rgeos)
      s$response <- x[gCentroid(loc)][1, ]
    }
    p <- ggplot(data = s, aes(x = date, y = response)) + geom_point() + theme_bw() + scale_x_date(limits = c(start, end))
    readline("Press any key to view time series plot: \n")
    print(p)
  }
  
  if(ggplot){
    return(p)
  } else {
    return(NULL)
  }
}
