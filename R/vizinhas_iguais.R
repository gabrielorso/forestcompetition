dados <- vizinhas <- NULL

# Função para contabilizar o número de vizinhas de mesma espécie, desconsiderando as bifurcações, quando presentes.

#' Cálculo de árvores vizinhas da mesma espécie
#'
#' Conta o número de árvores da mesma espécie dentre as `n` vizinhas mais próximas .
#'
#' @param alvo índice da árvore alvo dentro do banco de dados
#' @param n.vizinhas vetor numérico de um elemento, com o número de árvores vizinhas consideradas.
#' @param bif TRUE/FALSE que indica se as bifurcações serão levadas em conta. Padrão é FALSE
#' @param simplify TRUE/FALSE que indica se a função deve retornar apenas o número de vizinhas próximas ou um vetor contendo os id's das vizinhas de mesma espécie
#'
#' @details `bif` leva em conta também as bifurcações das árvores vizinhas, não somente possíveis bifurcações da árvore alvo
#'
#' @return Vetor numérico de um elemento com o número de vizinhas da mesma espécie, quando simplify = TRUE. Vetor caractere contendo os id's das ávores vizinhas de mesma espécie
#'
#' @export
#'
vizinhas_iguais <- function(alvo, n.vizinhas, bif = FALSE, simplify = TRUE) {

    #Árvore alvo
    id_alvo <- as.character(dados$id[alvo])
    nome_alvo <- as.character(dados$cient21[alvo])


    #Arvores vizinhas
    id_vizinhas <- as.character(dados$id[vizinhas[[alvo]]][-1])
    nome_vizinhas <- as.character(dados$cient21[vizinhas[[alvo]]][-1])

    #Removendo bifurcacoes
    if (bif == FALSE) {
        bifurcacoes <- stringr::str_count(id_vizinhas, ".B|.C|.D|.E|.F|.G|.H|.I")
        id_vizinhas <- id_vizinhas[bifurcacoes == 0]
        nome_vizinhas <- nome_vizinhas[bifurcacoes == 0]
    }

    id_vizinhas <- id_vizinhas[1:n.vizinhas]
    nome_vizinhas <- nome_vizinhas[1:n.vizinhas]

    if (simplify == TRUE) {
        return(sum(nome_vizinhas == nome_alvo))
    } else {
        return(nome_vizinhas)
    }
}

