

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
            dplyr::select(id, cient, cap_ant, cap, fuste, estrato, fito, copa)

        juntar <- dplyr::left_join(foo1, foo2, by = 'id', suffix = c('', '_ini')) %>%
            dplyr::left_join(foo2, by = c('id_ant' = 'id'), suffix = c('', '_ini2')) %>%
            dplyr::mutate(cient_ini = ifelse(!is.na(cient_ini2), cient_ini2, cient_ini),
                   cap_ant_ini = ifelse(!is.na(cap_ant_ini2), cap_ant_ini2, cap_ant_ini),
                   cap_ini = ifelse(!is.na(cap_ini2), cap_ini2, cap_ini),
                   fuste_ini = ifelse(!is.na(fuste_ini2), fuste_ini2, fuste_ini),
                   estrato_ini = ifelse(!is.na(estrato_ini2), estrato_ini2, estrato_ini),
                   fito_ini = ifelse(!is.na(fito_ini2), fito_ini2, fito_ini),
                   copa_ini = ifelse(!is.na(copa_ini2), copa_ini2, copa_ini),
            ) %>% dplyr::select(-tidyselect::ends_with('_ini2')) %>%
            dplyr::rename_with(~gsub('_ini', ano_inicial, .x), tidyselect::ends_with('_ini'))
    } else {

        foo2 <- m_inicial %>%
            dplyr::select(id, cient, cap, fuste, estrato, fito, copa)

        juntar <- dplyr::left_join(foo1, foo2, by = 'id', suffix = c('', '_ini')) %>%
            dplyr::left_join(foo2, by = c('id_ant' = 'id'), suffix = c('', '_ini2')) %>%
            dplyr::mutate(cient_ini = ifelse(!is.na(cient_ini2), cient_ini2, cient_ini),
                   cap_ini = ifelse(!is.na(cap_ini2), cap_ini2, cap_ini),
                   fuste_ini = ifelse(!is.na(fuste_ini2), fuste_ini2, fuste_ini),
                   estrato_ini = ifelse(!is.na(estrato_ini2), estrato_ini2, estrato_ini),
                   fito_ini = ifelse(!is.na(fito_ini2), fito_ini2, fito_ini),
                   copa_ini = ifelse(!is.na(copa_ini2), copa_ini2, copa_ini),
            ) %>% dplyr::select(-tidyselect::ends_with('_ini2')) %>%
            dplyr::rename_with(~gsub('_ini', ano_inicial, .x), tidyselect::ends_with('_ini'))
    }


    return(juntar)


}
