subsetScenesPlot <- function (..., bands = NULL, stretch = NULL, textcol = "white") 
{
  op <- par(mfrow = c(1, 1))
  scenes <- subsetScenes(...)
  fl <- sapply(scenes, FUN = function(x) names(x)[1])
  
  for (i in 1:length(scenes)) {
    print(fl[i])
    if (nlayers(scenes[[i]]) >= 3) {
      if (is.null(bands)) {
        plotRGB(scenes[[i]], 1, 2, 3, stretch = stretch)
      }
      else {
        plotRGB(scenes[[i]], bands[1], bands[2], bands[3], 
                stretch = stretch)
      }
      e <- extent(scenes[[i]])
      text(x = xmin(e) + xmax(e)/2, y = ymin(e) + 100*res(scenes[[i]])[1], labels = fl[i], col = textcol)
    }
    else if (nlayers(scenes[[i]]) == 2) {
      plot(raster(scenes[[i]], 1), main = fl[i])
    }
    else {
      plot(scenes[[i]], main = fl[i])
    }
    x <- list(...)[[1]]
    if (class(x) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
      plot(x, add = TRUE)
    }
    readline("Press any key to continue to next screen: \n")
    par(op)
  }
}