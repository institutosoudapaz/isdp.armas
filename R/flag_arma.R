#' Gera flag para arma artesanal
#'
#' Esta função adiciona uma coluna `flag_arma_artesanal` à tabela fornecida,
#' indicando se o tipo de arma é artesanal.
#'
#' @param tab Um data frame contendo uma coluna `tipo_formatado` com a descrição do tipo de arma (gerada pela função [depara_tipo()]).
#' 
#' @return Um data frame com uma nova coluna `flag_arma_artesanal`, que é TRUE se o tipo de arma for artesanal e FALSE caso contrário.
#' 
#' @export
gerar_flag_arma_artesanal <- function(tab) {
  tab |>
    dplyr::mutate(
      flag_arma_artesanal = stringr::str_detect(tipo_formatado, "artesanal")
    )
}

#' Gerar Flag de Arma
#'
#' Esta função gera uma flag de arma em uma tabela fornecida. A flag é definida como lógica e ajustada com base na compatibilidade do tipo de arma.
#'
#' @param tab Um data frame contendo a coluna `compatibilidade_tipo` e `flag_arma`.
#' 
#' @return Um data frame com a coluna `flag_arma` atualizada.
#' 
#' @export
#'
gerar_flag_arma <- function(tab) {
  tab |>
    dplyr::mutate(
      flag_arma = as.logical(flag_arma),
      flag_arma = dplyr::case_when(
        compatibilidade_tipo == "airsoft" ~ FALSE,
        TRUE ~ flag_arma
      )
    )
}
