#' Gerar Flag de Tipo de Arma Incompatível
#'
#' Esta função gera uma flag indicando se o tipo de arma é incompatível com o calibre informado.
#'
#' @param tab Um data frame contendo as colunas `tipo_formatado` 
#' (gerada pela função [depara_tipo()]) e `tipo_arma_calibre` (gerada pela 
#' função [`depara_calibre`]).
#'
#' @return Um data frame com uma nova coluna `flag_tipo_arma_incompativel_calibre` que indica se o tipo de arma é incompatível com o calibre.
#' 
#' @export
gerar_flag_tipo_arma_incompativel <- function(tab) {
  tab |>
    dplyr::mutate(
      flag_tipo_arma_incompativel_calibre = dplyr::case_when(
        is.na(tipo_formatado) ~ NA,
        is.na(tipo_arma_calibre) ~ TRUE,
        stringr::str_detect(tipo_arma_calibre, tipo_formatado) ~ FALSE,
        TRUE ~ TRUE
      )
    )
}

#' Gera a tabela final de tipos de armas
#'
#' Esta função recebe uma tabela e realiza transformações nas colunas para gerar a tabela final de tipos de armas.
#'
#' @param tab Um data frame contendo as colunas `tipo_formatado` 
#' (gerada pela função [depara_tipo()]) e `tipo_arma_calibre` (gerada pela 
#' função [`depara_calibre`]).
#' 
#' @return Data frame com a tabela final de tipos de armas, com colunas renomeadas e formatadas.
#' 
#' @export
gerar_arma_tipo_final <- function(tab) {
  tab |>
    dplyr::rename(
      compatibilidade_tipo = tipo_arma_calibre
    ) |> 
    dplyr::mutate(
      tipo_formatado = tolower(tipo_formatado),
      tipo_formatado = stringr::str_squish(tipo_formatado)
    )
}