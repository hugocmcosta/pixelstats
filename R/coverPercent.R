#'Computes coverage percentage of different land use classes from a raster image using moving windows.
#'
#' @param landScape A categorical raster layer object
#' @param landClass Landuse class for which coverage percentage will be calculated
#' @param windowSize size, in units of the landScape CRS, of the moving window to be created
#' @param windowForm the type of moving window: "circle" , "Gauss", "rectangle"
#'
#' @return a SpatRaster image
#' @export
#'

coverPercent <- function(landScape, landClass, windowSize, windowForm){

# calculo do raio da matriz focal
pxsize <- terra::res(landScape)
d <- mean(terra::res(landScape))*(windowSize/pxsize)
focmat <- terra::focalMat(landScape, d, type = windowForm)
# transformar tudo em 1 para encontrar valores exatos
focmat <- focmat/max(focmat)

clacover <- terra::focal(landScape, focmat,
                  function(landScape, landClass, ...){
                    mean(landScape %in% landClass, ...)
                  }, landClass == landClass, na.rm = TRUE)
return(clacover)

}

