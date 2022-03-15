#' Exemplo de uma remedição de parcela de floresta nativa no Sul do Brasil
#'
#' Um dataset contendo exemplo de remedição de parcela de floresta nativa no Sul do Brasil
#'
#' @format A data frame with 63 rows and 21 variables:
#' \describe{
#'   \item{bloco}{O bloco ou nome da parcela}
#'   \item{faixa}{Subdivisão do bloco em faixas}
#'   \item{ind_ant}{Numeração anterior da árvore. Só existe valor se a árvore ter tido seu número trocado}
#'   \item{ind}{Numeração atual da árvore}
#'   \item{bif}{Indica a bifurcação da árvore. Todas as árvores possuem pelo menos o fuste A}
#'   \item{y}{coordenada y da árvore, referente às coordenadas cartesianas do canto da parcela}
#'   \item{x}{coordenada x da árvore, referente às coordenadas cartesianas do canto da parcela}
#'   \item{vulgar}{Nome comum ou vernacular da árvore}
#'   \item{cient}{Nome científico da espécie arbórea}
#'   \item{fam}{Família da espécie}
#'   \item{cap_ant}{CAP da medição anterior. Essa variável só existe se for remedição}
#'   \item{cap}{CAP da árvore, medido à 1.3m acima do nível do solo}
#'   \item{fuste}{qualidade do fuste}
#'   \item{estrato}{Estrato de inserção da copa no dossel da floresta}
#'   \item{fito}{Qualidade fitossanitária da árvore}
#'   \item{copa}{Qualidade da copa da árvore}
#'   \item{obs}{Observações eventuais relevantes do indivíduo}
#'   \item{ref_y}{Coordenada N de referência do sistem de projeção (UTM 22S)}
#'   \item{ref_x}{Coordenada E de referência do sistema de projeção (UTM 22S)}
#'   \item{yy}{Combinação da coordenada local y com a de referência ref_y}
#'   \item{xx}{Combinação da coordenada local x com a de referência ref_x}
#' }
"remedicao"


#' Exemplo de uma medição de parcela de floresta nativa no Sul do Brasil
#'
#' Um dataset contendo exemplo de medição de parcela de floresta nativa no Sul do Brasil
#'
#' @format A data frame with 63 rows and 21 variables:
#' \describe{
#'   \item{bloco}{O bloco ou nome da parcela}
#'   \item{faixa}{Subdivisão do bloco em faixas}
#'   \item{ind}{Numeração atual da árvore}
#'   \item{bif}{Indica a bifurcação da árvore. Todas as árvores possuem pelo menos o fuste A}
#'   \item{y}{coordenada y da árvore, referente às coordenadas cartesianas do canto da parcela}
#'   \item{x}{coordenada x da árvore, referente às coordenadas cartesianas do canto da parcela}
#'   \item{vulgar}{Nome comum ou vernacular da árvore}
#'   \item{cient}{Nome científico da espécie arbórea}
#'   \item{fam}{Família da espécie}
#'   \item{cap}{CAP da árvore, medido à 1.3m acima do nível do solo}
#'   \item{fuste}{qualidade do fuste}
#'   \item{estrato}{Estrato de inserção da copa no dossel da floresta}
#'   \item{fito}{Qualidade fitossanitária da árvore}
#'   \item{copa}{Qualidade da copa da árvore}
#'   \item{obs}{Observações eventuais relevantes do indivíduo}
#'   \item{ref_y}{Coordenada N de referência do sistem de projeção (UTM 22S)}
#'   \item{ref_x}{Coordenada E de referência do sistema de projeção (UTM 22S)}
#'   \item{yy}{Combinação da coordenada local y com a de referência ref_y}
#'   \item{xx}{Combinação da coordenada local x com a de referência ref_x}
#' }
"medicao"
