
#' Importar base de dados
#'
#' @param arq Um vetor caractere de um elemento, com o caminho absoluto ou relativo do arquivo em formto .csv que será importado. O arquivo deve ter separador ';' e marcador de decimal ','.
#' @param col Um vetor caractere de um elemento com os códigos dos tipos das colunas similar ao argumento col_type das funções do pacote readr. Define a tipagem das colunas importadas.
#' @param re  TRUE/FALSE que indica que o arquivo importado é de uma remedição. Caso contrário, é a primeira medição. Padrão é TRUE para remedição.
#'
#' @return Um tibble
#' @export
#'
#' @examples
importar_base <- function(arq, col, re = TRUE) {
    if(re == TRUE) {
        df <- readr::read_csv2(arq,
                        skip_empty_rows = TRUE,  # Elimina linhas em branco
                        col_types = col) %>% dplyr::filter(!is.na(bloco)) %>%  # Também elimina linhas em branco
            dplyr::mutate(cap = ifelse(cap == '-', '0',  # Troca mortas por 0
                                ifelse(cap == 'NE', '1',  # Troca Não encontradas por 1
                                       ifelse(cap == 'NM', '2', cap))),  # Troca não medidas por 2
                   cap_ant = ifelse(cap_ant == '-', '0',  # Mesma coisa para o cap anterior
                                    ifelse(cap_ant == 'NE', '1',
                                           ifelse(cap_ant == 'NM', '2', cap_ant))),
                   id = paste0(bloco, '.', faixa, '/', ind, bif),  # Cria código para a árvore
                   id_ant = ifelse(is.na(ind_ant), NA,  # Cria código antigo para a árvore, se houver
                                   paste0(bloco, '.', faixa, '/', ind_ant, bif))
            )

        # 0 = '-' ; 1 = 'NE' (Não encontrada) ; 1 = NM (Não medida, por risco alto, etc.)

        # Troca decimal para ponto e converte em número
        df$cap <- as.numeric(gsub(',', '.', df$cap))
        df$cap_ant <- as.numeric(gsub(',', '.', df$cap_ant))
    } else {
        df <- readr::read_csv2(arq,
                        skip_empty_rows = TRUE,
                        col_types = col) %>% dplyr::filter(!is.na(bloco)) %>%
            dplyr::mutate(cap = ifelse(cap == '-', '0',
                                ifelse(cap == 'NE', '1',
                                       ifelse(cap == 'NM', '2', cap))),
                   id = paste0(bloco, '.', faixa, '/', ind, bif))

        # 0 = '-' ; 1 = 'NE' (Não encontrada) ; 1 = NM (Não medida, por risco alto, etc.)

        df$cap <- as.numeric(gsub(',', '.', df$cap))

    }

    return(df)
}
