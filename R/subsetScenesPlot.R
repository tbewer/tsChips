#' @title Plot automatically subsetted scenes
#' 
#' @description plot scenes selected using \code{subsetScenes}. These can either by single layer or multilayer rasters (or a combination of the two), in which case this function will plot accordingly.
#' 
#' @param ... Arguments to be passed to \code{\link{subsetScenes}}
#' @param bands Numeric. Optional: see \code{details}. 
#' 
#' @details If \code{subsetScenes} returns a multi-layered raster with \code{nlayers > 3}, \code{bands} determines which bands should be plotted (and in which order, according to RGB compositing). If ignored, the first 3 bands will be plotted in that order. If there are 2 layers, the first will be plotted as a single layer. 
#'
#' @return \code{NULL}, with the side effect of plotting outcome of \code{subsetScenes}
#' 
#' @author Ben DeVries
#' 
#' @import raster
#' @import sp
#' @export
#' 

subsetScenesPlot <- function(..., bands = NULL, stretch = NULL, textcol = "white") {
  
  op <- par(mfrow = c(1, 1))
  scenes <- subsetScenes(...)
  
  # get scene filenames
  ## TODO: replace this chunk with more elegant regex!
  fl <- sapply(scenes, FUN=function(x) x@file@name)
  pos <- sapply(fl, FUN=function(x) gregexpr("/", x))
  n <- sapply(pos, FUN=function(x) x[length(x)] + 1)
  newfl <- substr(fl, n, nchar(str))
  
  for(i in 1:length(scenes)) {
    if(nlayers(scenes[[i]]) >= 3){
      if(is.null(bands)) {
        plotRGB(scenes[[i]], 1, 2, 3, stretch = stretch)
      } else {
        plotRGB(scenes[[i]], bands[1], bands[2], bands[3], stretch = stretch)
      }
      text(x = (xmin(extent(scenes[[i]])) + xmax(extent(scenes[[i]])))/2, y = ymin(extent(scenes[[i]])) + 2*res(scenes[[i]])[1], labels = newfl[i], col = textcol)
    } else if(nlayers(scenes[[i]]) == 2){
      plot(raster(scenes[[i]], 1))
    } else {
      plot(scenes[[i]])
    }
    
    # function to add spatial data (if present)
    x <- list(...)[[1]]
    if(class(x) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
      plot(x, add=TRUE)
    }
    
    readline("Press any key to continue to next screen: \n")
    par(op)
  }
  
}