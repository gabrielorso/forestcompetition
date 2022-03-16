cap_ant_ini <- cap_ant_ini2 <- cap_ini <- cap_ini2 <- cient <- cient_ini <- cient_ini2 <- copa <- copa_ini <- copa_ini2 <- estrato <- estrato_ini <- estrato_ini2 <- fito <- fito_ini <- fito_ini2 <- fuste <- fuste_ini <- fuste_ini2 <- id <- id_ant <- x <- y <- NULL

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
        dplyr::select(bloco, id, id_ant, y, x,
               cient, cap_ant, cap, fuste,
               estrato, fito, copa)  # Seleciona apenas as colunas necessárias

    if('cap_ant' %in% names(m_inicial)) {
        # Se existir a coluna cap_ant no inventário inicial, então ele já é remedição.

        foo2 <- m_inicial %>%
            dplyr::select(id, cient, cap_ant, cap, fuste, estrato, fito, copa)

        juntar <- dplyr::left_join(foo1, foo2, by = 'id', suffix = c('', '_ini')) %>%  # Juntando à esquerda pelo id da árvore
            dplyr::left_join(foo2, by = c('id_ant' = 'id'), suffix = c('', '_ini2')) %>%  # Juntando à esquerda pelo id anterior da árvore. Note que somente as árvores com id anterior terão valor nas colunas, o resto será NA.
            dplyr::mutate(cient_ini = ifelse(!is.na(cient_ini2), cient_ini2, cient_ini),  # Se existir alguma coisa na coluna ._ini2, então a árvore teve troca de numeração, e as informações da numeração anterior (._ini2) devem ser utilizadas.
                   cap_ant_ini = ifelse(!is.na(cap_ant_ini2), cap_ant_ini2, cap_ant_ini),
                   cap_ini = ifelse(!is.na(cap_ini2), cap_ini2, cap_ini),
                   fuste_ini = ifelse(!is.na(fuste_ini2), fuste_ini2, fuste_ini),
                   estrato_ini = ifelse(!is.na(estrato_ini2), estrato_ini2, estrato_ini),
                   fito_ini = ifelse(!is.na(fito_ini2), fito_ini2, fito_ini),
                   copa_ini = ifelse(!is.na(copa_ini2), copa_ini2, copa_ini),
            ) %>% dplyr::select(-tidyselect::ends_with('_ini2')) %>%  # Retirando as colunas ._ini2. As informações foram repassadas para as colunas ._ini
            dplyr::rename_with(~gsub('_ini', ano_inicial, .x), tidyselect::ends_with('_ini'))  # Alterando o sufixo _ini para o sufixo do respectivo ano de medição
    } else {
        # Caso quando não há cap_ant, caracterizando o data.frame como um banco de dados de primeira medição.

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
