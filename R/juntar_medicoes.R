
#' Juntar Medições de inventários sucessivos
#'
#' As funções juntar_medicoes() e juncao_composta() são importantes pois as árvores não seguem a mesma ordem nas fichas ao longo dos anos, nem mantém a mesma numeração entre inventários. Isso impede que seja feito um simples 'recortar e colar' das variáveis medidas em cada ano. É necessário que as árvores tenham um código único, como uma âncora, de forma que as informações de um código sejam combinadas única e exclusivamente com as informações do mesmo código entre inventários sucessivos.
#' Para as árvores com mudança de numeração entre inventários, primeiro a função pareia o mesmo código (coluna 'id') das árvores em dois inventários distintos. Caso não encontre o mesmo código nos dois inventários, então a função busca a âncora no código antigo (coluna 'id_ant').
#'
#' @param m_final Um data frame contendo o banco de dados da medição posterior
#' @param m_inicial Um data frame contendo o banco de dados da medição inicial
#' @param ano_inicial Um vetor caractere de um elemento indicando o ano ('YYYY') da medição inicial
#'
#' @return Um tibble com as medições realizadas nos dois inventários
#' @export
#'
juntar_medicoes <- function(m_final, m_inicial, ano_inicial) {
    # Função que auxilia na junção de duas bases de dados 'alinhando' as árvores de diferentes inventários
    # pelo id ou id anterior, caso a árvore tenha trocado de número.

    # m_final é um data.frame contendo o banco de dados posterior
    # m_inicial é um data.frame contendo o banco de dados anterior
    # ano_inicial é um caractere informando o ano de medição do banco de dados anterior

    foo1 <- m_final %>%
        dplyr::select(.data[['bloco']], .data$id, .data$id_ant, .data$y, .data$x,
               .data$cient, .data$cap_ant, .data$cap, .data$fuste,
               .data$estrato, .data$fito, .data$copa)  # Seleciona apenas as colunas necessárias

    if('cap_ant' %in% names(m_inicial)) {
        # Se existir a coluna cap_ant no inventário inicial, então ele já é remedição.

        foo2 <- m_inicial %>%
            dplyr::select(.data$id, .data$cient, .data$cap_ant, .data$cap, .data$fuste, .data$estrato, .data$fito, .data$copa)

        juntar <- dplyr::left_join(foo1, foo2, by = 'id', suffix = c('', '_ini')) %>%  # Juntando à esquerda pelo id da árvore
            dplyr::left_join(foo2, by = c('id_ant' = 'id'), suffix = c('', '_ini2')) %>%  # Juntando à esquerda pelo id anterior da árvore. Note que somente as árvores com id anterior terão valor nas colunas, o resto será NA.
            dplyr::mutate(cient_ini = ifelse(!is.na(.data$cient_ini2), .data$cient_ini2, .data$cient_ini),  # Se existir alguma coisa na coluna ._ini2, então a árvore teve troca de numeração, e as informações da numeração anterior (._ini2) devem ser utilizadas.
                   cap_ant_ini = ifelse(!is.na(.data$cap_ant_ini2), .data$cap_ant_ini2, .data$cap_ant_ini),
                   cap_ini = ifelse(!is.na(.data$cap_ini2), .data$cap_ini2, .data$cap_ini),
                   fuste_ini = ifelse(!is.na(.data$fuste_ini2), .data$fuste_ini2, .data$fuste_ini),
                   estrato_ini = ifelse(!is.na(.data$estrato_ini2), .data$estrato_ini2, .data$estrato_ini),
                   fito_ini = ifelse(!is.na(.data$fito_ini2), .data$fito_ini2, .data$fito_ini),
                   copa_ini = ifelse(!is.na(.data$copa_ini2), .data$copa_ini2, .data$copa_ini),
            ) %>% dplyr::select(-tidyselect::ends_with('_ini2')) %>%  # Retirando as colunas ._ini2. As informações foram repassadas para as colunas ._ini
            dplyr::rename_with(~gsub('_ini', ano_inicial, .x), tidyselect::ends_with('_ini'))  # Alterando o sufixo _ini para o sufixo do respectivo ano de medição
    } else {
        # Caso quando não há cap_ant, caracterizando o data.frame como um banco de dados de primeira medição.

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
