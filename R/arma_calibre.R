#' Gerar Calibre Final da Arma
#'
#' Esta função processa uma tabela de armas para gerar a coluna `arma_calibre_final`
#' a partir de regras relacionadas ao `calibre_formatado_final` e ao `tipo_formatado`.
#'
#' @param tab Um data frame contendo as colunas `calibre_formatado_final` e
#' `tipo_formatado` (colunas criadas pela função [depara_calibre()] e
#' [depara_tipo()] respectivamente).
#'
#' @return Um data frame com uma coluna adicional `arma_calibre_final` que contém o calibre final da arma.
#'
#' @details As condições para determinar o calibre final são as seguintes:
#' - Se `calibre_formatado_final` for ".32" e `tipo_formatado` for "espingarda", o calibre final é "32 gauge".
#' - Se `calibre_formatado_final` for ".32" e `tipo_formatado` for "revolver" ou "garrucha", o calibre final é ".32 S&W long".
#' - Se `calibre_formatado_final` for ".32" e `tipo_formatado` for "pistola", "carabina" ou "submetralhadora", o calibre final é ".32 acp".
#' - Se `calibre_formatado_final` terminar com "32", o calibre final é ".32 S&W long".
#' - Caso contrário, o calibre final é o valor de `calibre_formatado_final`.
#'
#' A coluna `arma_calibre_final` resultante é então convertida para maiúsculas.
#'
#' @export
gerar_arma_calibre_final <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_calibre_final = dplyr::case_when(
        calibre_formatado_final == ".32" &
          tipo_formatado == "espingarda" ~ "32 gauge",
        calibre_formatado_final == ".32" &
          tipo_formatado %in% c("revolver", "garrucha") ~ ".32 S&W long",
        calibre_formatado_final == ".32" &
          tipo_formatado %in% c("pistola", "carabina", "submetralhadora") ~ ".32 acp",
        stringr::str_detect(calibre_formatado_final, "32$") ~ ".32 S&W long",
        TRUE ~ calibre_formatado_final
      ),
      arma_calibre_final = toupper(arma_calibre_final)
    )
}
