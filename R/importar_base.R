bloco <- cap <- cap_ant <- faixa <- ind <- bif <- ind_ant <- NULL


#' Importar base de dados
#'
#' @param arq Um vetor caractere de um elemento, com o caminho absoluto ou relativo do arquivo em formto .csv que será importado. O arquivo deve ter separador ';' e marcador de decimal ','.
#' @param col Um vetor caractere de um elemento com os códigos dos tipos das colunas similar ao argumento col_type das funções do pacote readr. Define a tipagem das colunas importadas. Por padrão vale 'cciicnncccccccccciinn' para remedições e 'ccicnnccccccccciinn' para primeira medição
#' @param re  TRUE/FALSE que indica que o arquivo importado é de uma remedição. Caso contrário, é a primeira medição. Padrão é TRUE para remedição.
#'
#' @details O arquivo a ser importado deve obrigatoriamente conter as colunas **bloco**, **cap**, **ind** para o caso de primeira medição. Quando for remedição, acrescentam-se as colunas **ind_ant** e **cap_ant** como obrigatórias. Um exemplo de um banco de dados adequado para medição e remedição pode ser visualizado consultando `head(medicao)` e `head(remedicao)`
#'
#' @return Um tibble com o banco de dados formatado da forma apropriada. Veja os datasets `data(medicao)` e `data(remedicao)`.
#'
#' @export
importar_base <- function(arq, col = 'standard', re = TRUE) {
    if(col == 'standard') {
        if(re == TRUE) {coluna <-'cciicnncccccccccciinn'} else {coluna <- 'ccicnnccccccccciinn'}
    } else {coluna <- col}

    if(re == TRUE) {
        df <- readr::read_csv2(arq,
                        skip_empty_rows = TRUE,  # Elimina linhas em branco
                        col_types = coluna) %>% dplyr::filter(!is.na(bloco)) %>%  # Também elimina linhas em branco
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
                        col_types = coluna) %>% dplyr::filter(!is.na(bloco)) %>%
            dplyr::mutate(cap = ifelse(cap == '-', '0',
                                ifelse(cap == 'NE', '1',
                                       ifelse(cap == 'NM', '2', cap))),
                   id = paste0(bloco, '.', faixa, '/', ind, bif))

        # 0 = '-' ; 1 = 'NE' (Não encontrada) ; 1 = NM (Não medida, por risco alto, etc.)

        df$cap <- as.numeric(gsub(',', '.', df$cap))

    }

    return(df)
}
