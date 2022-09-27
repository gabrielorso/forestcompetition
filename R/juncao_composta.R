

#' Junta medições já combinadas com uma nova base de dados
#'
#' #' As funções juntar_medicoes() e juncao_composta() são importantes pois as árvores não seguem a mesma ordem nas fichas ao longo dos anos, nem mantém a mesma numeração entre inventários. Isso impede que seja feito um simples 'recortar e colar' das variáveis medidas em cada ano. É necessário que as árvores tenham um código único, como uma âncora, de forma que as informações de um código sejam combinadas única e exclusivamente com as informações do mesmo código entre inventários sucessivos.
#' Para as árvores com mudança de numeração entre inventários, primeiro a função pareia o mesmo código (coluna 'id') das árvores em dois inventários distintos. Caso não encontre o mesmo código nos dois inventários, então a função busca a âncora no código antigo (coluna 'id_ant').
#'
#' @param m_composto Um data frame retornado de juntar_medicoes().
#' @param m_inicial Um data frame com uma medição anterior para ser combinado
#' @param ano_inicial Vetor caractere de um elemento contendo o ano ('YYYY') da medição inicial
#'
#' @return Um tibble com a combinação dos data frames
#' @export
#'
juncao_composta <- function(m_composto, m_inicial, ano_inicial) {
    # Função auxiliar na combinação de uma base de dados já combinada com uma nova base de dados
    # m_composto é um data.frame com combinação de duas ou mais medições
    # m_inicial é um data frame com uma medição anterior a ser combinada.
    # ano_inicial é um caractere com o ano de medição

    foo1 <- m_composto

    if('cap_ant' %in% names(m_inicial)) {

        foo2 <- m_inicial %>%
            dplyr::select(.data$id, .data$cient, .data$cap_ant, .data$cap, .data$fuste, .data$estrato, .data$fito, .data$copa)

        juntar <- dplyr::left_join(foo1, foo2, by = 'id', suffix = c('', '_ini')) %>%
            dplyr::left_join(foo2, by = c('id_ant' = 'id'), suffix = c('', '_ini2')) %>%
            dplyr::mutate(cient_ini = ifelse(!is.na(.data$cient_ini2), .data$cient_ini2, .data$cient_ini),
                   cap_ant_ini = ifelse(!is.na(.data$cap_ant_ini2), .data$cap_ant_ini2, .data$cap_ant_ini),
                   cap_ini = ifelse(!is.na(.data$cap_ini2), .data$cap_ini2, .data$cap_ini),
                   fuste_ini = ifelse(!is.na(.data$fuste_ini2), .data$fuste_ini2, .data$fuste_ini),
                   estrato_ini = ifelse(!is.na(.data$estrato_ini2), .data$estrato_ini2, .data$estrato_ini),
                   fito_ini = ifelse(!is.na(.data$fito_ini2), .data$fito_ini2, .data$fito_ini),
                   copa_ini = ifelse(!is.na(.data$copa_ini2), .data$copa_ini2, .data$copa_ini),
            ) %>% dplyr::select(-tidyselect::ends_with('_ini2')) %>%
            dplyr::rename_with(~gsub('_ini', ano_inicial, .x), tidyselect::ends_with('_ini'))
    } else {

        foo2 <- m_inicial %>%
            dplyr::select(.data$id, .data$cient, .data$cap, .data$fuste, .data$estrato, .data$fito, .data$copa)

        juntar <- dplyr::left_join(foo1, foo2, by = 'id', suffix = c('', '_ini')) %>%
            dplyr::left_join(foo2, by = c('id_ant' = 'id'), suffix = c('', '_ini2')) %>%
            dplyr::mutate(cient_ini = ifelse(!is.na(.data$cient_ini2), .data$cient_ini2, .data$cient_ini),
                   cap_ini = ifelse(!is.na(.data$cap_ini2), .data$cap_ini2, .data$cap_ini),
                   fuste_ini = ifelse(!is.na(.data$fuste_ini2), .data$fuste_ini2, .data$fuste_ini),
                   estrato_ini = ifelse(!is.na(.data$estrato_ini2), .data$estrato_ini2, .data$estrato_ini),
                   fito_ini = ifelse(!is.na(.data$fito_ini2), .data$fito_ini2, .data$fito_ini),
                   copa_ini = ifelse(!is.na(.data$copa_ini2), .data$copa_ini2, .data$copa_ini),
            ) %>% dplyr::select(-tidyselect::ends_with('_ini2')) %>%
            dplyr::rename_with(~gsub('_ini', ano_inicial, .x), tidyselect::ends_with('_ini'))
    }


    return(juntar)


}
