camada_sf <- dados_sf <- camada_sf <- camada <- NULL

#' Encontra e calcula a distancia das arvores vizinhas
#'
#' Encontra e calcula a distancia das arvores vizinhas, dado um ponto de uma calula raster.
#'
#' @param celula namero da calula do da camada raster
#' @param camada_inferior camada do tipo sf de onde serao retiradas as calulas
#' @param camada_superior camada do tipo sf de onde virao os pontos que se deseja saber a distancia
#' @param camada_raster camada do tipo raster que fornece as coordenadas das calulas
#' @param ... argumentos adicionais para serem passados a funcao st_nn
#'
#' @return retorna o mesmo tipo da funcao st_nn()
#' @export
vizinhos_cell <- function(celula, camada_inferior = camada_sf, camada_superior = dados_sf,
                          camada_raster = camada, ...) {

    if(length(celula) == 1) {
        ponto = as.numeric(raster::xyFromCell(camada_raster, celula))
    } else {
        stop('ponto deve ser o numero da celula')
    }

    viz <- nngeo::st_nn(x = camada_inferior[camada_inferior$X %in% ponto[1] & camada_inferior$Y %in% ponto[2],]$geometry,
                 y = camada_superior, progress = FALSE, ...)

    return(viz)

}
