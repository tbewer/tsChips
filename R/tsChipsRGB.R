#' @title Plot TS Chips (RGB version)
#' 
#' @description Plot image TS chips from 3 raster bricks (as RGB composites) based on a location, area buffer, and time buffer
#' 
#' @param xr/xg/xb RasterBrick. Image time series bricks representing the red, green and blue channel, respectively
#' @param loc Location. Can be a vector of length 2 representing the x,y coordinates, or a SpatialPolygons or SpatialPoints object of \code{nrow = 1} (the first row will be taken if \code{nrow(loc) > 1}), or an extent object (which will be extended if a buffer > 0 is given; see below)
#' @param buff Numeric. Number of pixels to buffer the location in all directions. A higher buffer will essentially zoom out.
#' @param start Date. OptionaL: earliest date ("yyyy-dd-mm") to display.
#' @param end Date. Optional: latest date ("yyyy-dd-mm") to display.
#' @param percNA Numeric. Maximum allowable \% NA in the cropped image chips
#' @param nc/nr Numeric. Number of columns and rows to plot, respectively. If the number of layers is greater than \code{nc*nr}, a screen prompt will lead to the next series of plots. These cannot exceed 4.
#' @param ggplot Logical. Produce a ggplot time series plot object?
#' @param cores Numeric. Number of cores to use for pre-processing (useful for cropping step). Cannot exceed 3.
#' @param textcol Character. Colour of text showing image date (can also be hexadecimal)
#' @param ... Arguments to be passed to \code{\link{plotRGB}}
#' 
#' @return \code{NULL} if \code{ggplot = FALSE} or an object of class \code{ggplot} if \code{ggplot = TRUE}, with the side effect of time series chips being plotted in both cases.
#' 
#' @author Ben DeVries
#' 
#' @import bfastSpatial
#' @import RColorBrewer
#' @import ggplot2
#' @import reshape2
#' @export
#' 
#' @references
#' Cohen, W. B., Yang, Z., Kennedy, R. (2010). Detecting trends in forest disturbance and recovery using yearly Landsat time series: 2. TimeSync - Tools for calibration and validation. Remote Sensing of Environment, 114(12), 2911-2924.
#' 


tsChipsRGB <- function(xr, xg, xb, loc, start = NULL, end = NULL, buff = 17, percNA = 20, nc = 3, nr = 3, ggplot = FALSE, cores = 1, textcol = "white", ...) {
  
  # check that all bricks have the same number of layers and are comparable
  if(!compareRaster(xr, xg, xb))
    stop("Input RGB rasterBricks do not compare")
  if(length(unique(nlayers(xr), nlayers(xg), nlayers(xb))) > 1)
    stop("Input RGB rasterBricks have different number of layers")
    
  x <- list(R = xr, G = xg, B = xb)
  
  # get sceneinfo
  s <- getSceneinfo(names(x$R))
  
  # reformat buffer using image resolution
  buff <- buff * res(x$R)[1]
  
  # check location format and make a buffered extent object
  if(class(loc) == "numeric" & length(loc) != 2){
    stop("loc should be either a numeric vector of length 2 or a spatial object (polygon, points or extent).")
  } else if(class(loc) == "numeric"){
    e <- extent(c(loc[1] - buff, loc[1] + buff, loc[2] - buff, loc[2] + buff))
  } else if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialPoints", "SpatialPointsDataFrame")){
    if(length(loc) > 1){
      warning("only taking the 1st feature of loc")
      loc <- loc[1, ]
    }
    e <- extent(loc)
    e <- extent(c(xmin(e) - buff, xmax(e) + buff, ymin(e) - buff, ymax(e) + buff))
  } else if(class(loc) == "extent"){
    e <- loc
    e <- extent(c(xmin(e) - buff, xmax(e) + buff, ymin(e) - buff, ymax(e) + buff))
  }
  
  # crop input bricks
  if(cores > 1) {
    if(cores > 3)
      cores <- 3
    require(doMC)
    registerDoMC(cores = cores)
    xe <- foreach(i = 1:length(x)) %dopar% {
      crop(x[[i]], e)
    }
    # start and end dates
    if(!is.null(start)){
      start <- as.Date(start)
      xe <- foreach(i = 1:length(xe)) %dopar% {
        raster::subset(xe[[i]], subset = which(s$date >= start))
      }
    } else {
      start <- as.Date(min(s$date)) # to be used in ggplot later
    }
    if(!is.null(end)){
      end <- as.Date(end)
      xe <- foreach(i = 1:length(x)) %dopar% {
        raster::subset(xe[[i]], subset = which(s$date <= end))
      }
    } else {
      end <- as.Date(max(s$date)) # to be used in ggplot later
    }    
  
  } else {
    
    xe <- vector("list", 3)
    for(i in 1:length(x)){
      xe[[i]] <- crop(x[[i]], e)
    }
    # start and end dates
    if(!is.null(start)){
      start <- as.Date(start)
      for(i in 1:length(xe)){
        xe[[i]] <- raster::subset(xe[[i]], subset = which(s$date >= start))
      }
    } else {
      start <- as.Date(min(s$date)) # to be used in ggplot later
    }
    if(!is.null(end)){
      end <- as.Date(end)
      for(i in 1:length(x)){
        xe[[i]] <- raster::subset(xe[[i]], subset = which(s$date <= end))
      }
    } else {
      end <- as.Date(max(s$date)) # to be used in ggplot later
    }
  }
  se <- getSceneinfo(names(xe[[1]]))
  
  # reorder scenes
  for(i in 1:length(xe)){
    xe[[i]] <- raster::subset(xe[[i]], subset = order(se$date))
  }
  se <- getSceneinfo(names(xe[[1]]))
  
  # filter out scenes with too many NA's
  # done on 1st band, assuming mask has been applied uniformly
  if(percNA < 100){
    nas <- sapply(freq(xe[[1]]), FUN=function(x) as.numeric(x[is.na(x[, 1]), 2] / ncell(xe[[1]]) * 100))
    nas[which(sapply(nas, length) == 0)] <- 0
    nas <- unlist(nas)
    for(i in 1:length(x)){
      if(percNA == 0){
        xe[[i]] <- raster::subset(xe[[i]], subset = which(nas == percNA))
      } else {
        xe[[i]] <- raster::subset(xe[[i]], subset = which(nas < percNA))
      }
    }
  }
  
  # final sceneinfo data.frame
  se <- getSceneinfo(names(xe[[1]]))
  
  
  # function to add spatial data (if present)
  if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialPoints", "SpatialPointsDataFrame")){
    addfun <- function() plot(loc, add=TRUE)
  } else {
    addfun <- function() NULL
  }
  
  # plots on separate screens if needed
  op <- par(mfrow = c(nr, nc))
  pps <- nc * nr
  nscreens <- ceiling(nlayers(xe[[1]]) / pps)
  
  for(i in seq(1, nlayers(xe[[1]]), by = pps)){
    if((nlayers(xe[[1]]) - i) <= pps){
      xes <- lapply(xe, FUN=function(y) raster::subset(y, subset = c(i:nlayers(y))))
      for(j in 1:nlayers(xes[[1]])){
        b <- brick(raster(xes[[1]], j), raster(xes[[2]], j), raster(xes[[3]], j))
        err <- try({
          plotRGB(b, 1, 2, 3, addfun=addfun, ...)
          text(x = (xmin(e) + xmax(e))/2, y = ymin(e) + 2*res(xr)[1], labels = se$date[i + j -1], col = textcol)
        }, silent = TRUE)
        if(class(err) == "try-error")
          plot.new()
      }
      par(op)
    } else {
      xes <- lapply(xe, FUN=function(y) raster::subset(y, subset = c(i:(i + pps - 1))))
      for(j in 1:nlayers(xes[[1]])){
        b <- brick(raster(xes[[1]], j), raster(xes[[2]], j), raster(xes[[3]], j))
        err <- try({
          plotRGB(b, 1, 2, 3, addfun=addfun, ...)
          text(x = (xmin(e) + xmax(e))/2, y = ymin(e) + 2*res(xr)[1], labels = se$date[i + j -1], col = textcol)
        }, silent = TRUE)
        if(class(err) == "try-error")
          plot.new()
      }
          
      readline("Press any key to continue to next screen: \n")
    }
  }
  ## works, but still have to label dates!
  
  # TODO:
  # 1. add option to plot one or more RE scenes at end of ts if they are available.
  
  if(ggplot){
    require(ggplot2)
    if(is.numeric(loc)){
      s$R <- x$R[cellFromXY(x$R, loc)][1, ]
      s$G <- x$G[cellFromXY(x$G, loc)][1, ]
      s$B <- x$B[cellFromXY(x$B, loc)][1, ]
    } else if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialPoints", "SpatialPointsDataFrame")){
      s$R <- apply(extract(x$R, loc)[[1]], 2, mean)
      s$G <- apply(extract(x$G, loc)[[1]], 2, mean)
      s$B <- apply(extract(x$B, loc)[[1]], 2, mean)
    }
    s <- na.omit(s)
    s <- melt(s, measure.vars = c("R", "G", "B"))
    names(s) <- c("sensor", "path", "row", "date", "channel", "value")
    
    p <- ggplot(data = s, aes(x = date, y = value)) + theme_bw() + scale_x_date(limits = c(start, end)) + geom_line(lty = 2, aes(colour = channel))
    readline("Press any key to view time series plot: \n")
    print(p)
  }
  
  if(ggplot){
    return(p)
  } else {
    return(NULL)
  }
}
