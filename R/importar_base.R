

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
#' @importFrom rlang .data
importar_base <- function(arq, col = 'standard', re = TRUE) {
    if(col == 'standard') {
        if(re == TRUE) {coluna <-'cciicnncccccccccciinn'} else {coluna <- 'ccicnnccccccccciinn'}
    } else {coluna <- col}

    if(re == TRUE) {
        df <- readr::read_csv2(arq,
                        skip_empty_rows = TRUE,  # Elimina linhas em branco
                        col_types = coluna) %>% dplyr::filter(!is.na(.data$bloco)) %>%  # Também elimina linhas em branco
            dplyr::mutate(cap = ifelse(.data$cap == '-', '0',  # Troca mortas por 0
                                ifelse(.data$cap == 'NE', '1',  # Troca Não encontradas por 1
                                       ifelse(.data$cap == 'NM', '2', .data$cap))),  # Troca não medidas por 2
                   cap_ant = ifelse(.data$cap_ant == '-', '0',  # Mesma coisa para o cap anterior
                                    ifelse(.data$cap_ant == 'NE', '1',
                                           ifelse(.data$cap_ant == 'NM', '2', .data$cap_ant))),
                   id = paste0(.data$bloco, '.', .data$faixa, '/', .data$ind, .data$bif),  # Cria código para a árvore
                   id_ant = ifelse(is.na(.data$ind_ant), NA,  # Cria código antigo para a árvore, se houver
                                   paste0(.data$bloco, '.', .data$faixa, '/', .data$ind_ant, .data$bif))
            )

        # 0 = '-' ; 1 = 'NE' (Não encontrada) ; 1 = NM (Não medida, por risco alto, etc.)

        # Troca decimal para ponto e converte em número
        df$cap <- as.numeric(gsub(',', '.', df$cap))
        df$cap_ant <- as.numeric(gsub(',', '.', df$cap_ant))
    } else {
        df <- readr::read_csv2(arq,
                        skip_empty_rows = TRUE,
                        col_types = coluna) %>% dplyr::filter(!is.na(.data$bloco)) %>%
            dplyr::mutate(cap = ifelse(.data$cap == '-', '0',
                                ifelse(.data$cap == 'NE', '1',
                                       ifelse(.data$cap == 'NM', '2', .data$cap))),
                   id = paste0(.data$bloco, '.', .data$faixa, '/', .data$ind, .data$bif))

        # 0 = '-' ; 1 = 'NE' (Não encontrada) ; 1 = NM (Não medida, por risco alto, etc.)

        df$cap <- as.numeric(gsub(',', '.', df$cap))

    }

    return(df)
}
