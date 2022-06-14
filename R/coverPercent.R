#'Computes coverage percentage of different land use classes from a raster image using buffers of different sizes.
#'
#' @param Raster A categorical raster layer object , with projected CRS
#' @param surveySites Spatial points object with projected CRS. The first columm of its atribute table must contain the point identification
#' @param bufferSizes size in meters of the buffer to be created around each survey site
#'
#' @return data frame
#' @export
#'
#' @examples
coverPercent <- function(Raster, surveySites, bufferSizes){
  # determinar o numero de pontos e a quantidade de buffers que serao criados
  nSites <- length(surveySites)
  nBuffers <- length(bufferSizes)
  # matriz vazia para guardar os resultados
  # o n de linhas devera ser o mesmo n de pontos
  # o id das colunas indicara o tamanho dos buffers
  m <- matrix(nrow = nSites,
              ncol = nBuffers)
  colnames(m) <- paste(bufferSizes,"m")
  # criar uma lista vazia para armazenar cada matriz resultante
  out_list <- list()
  # inciar o primeiro loop para automatizar para o n de pontos
  for (s in 1 : nSites) {
    site1<- surveySites[s,]
    # o segundo loop automatiza para o n de buffers
    for (b in 1 : nBuffers) {
      # fazer um buffer em cada ponto e de cada tamanho indicado

      buffer1 <- raster::buffer(site1, width = bufferSizes[b])
      # fazer um mask de cada buffer no raster
      raster1 <- raster::mask(x = Raster, mask = buffer1, inverse=F)
      # calcula a frequencia de cada classe em cada raster de buffer
      f <- raster::trim(raster::freq(raster1))
      # o terceiro loop automatiza o calculo da porcentagem de cada classe em cada buffer
      for (lc in 1 : length(unique(Raster@data@values))) {
        classe <- sum(f[f[,1] %in% lc, 2])
        #  total de pixels nos rasters com mask dos buffers
        total <- sum(f[!is.na(f[ ,1]),2])
        # a porcentagem da classe "lc" em cada buffer
        p.classe <- classe / total
        # gravar os resultados , pontos nas linhas e buffers nas colunas
        m [s,b] <- p.classe
        # as matrizes "landcover" de cada classe serão armazenadas em uma lista
        out_list[[lc]] <- data.frame(Site_ID = surveySites@data[,1], Class = lc, m,check.names = F)
        # dataframe com o resultado final indicando os pontos e as classes
        landcover <- do.call('rbind', out_list)
      }
    }
  }
  return(landcover)
}


