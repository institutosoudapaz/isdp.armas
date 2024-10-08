#' Gerar Rubrica Formatada
#'
#' Esta função formata a tabela de acordo com a base especificada.
#'
#' @param tab DataFrame. A tabela que será formatada.
#' @param base Character. A base de dados que será utilizada para formatar a tabela. Pode ser "rj" ou "rj_complementar".
#'
#' @return DataFrame. A tabela formatada de acordo com a base especificada.
#'
#' @export
gerar_rubrica_formatada <- function(tab, base) {
  if (base == "rj") {
    tab |>
      dplyr::rename(
        rubrica_formatada = crime_formatado,
        rubrica_formatada_do = crime_formatado_do
      )
  } else if (base == "rj_complementar") {
    tab |>
      depara_crime(
        nome_coluna = "tipo_delito"
      ) |>
      dplyr::rename(
        rubrica_formatada = crime_formatado
      )
  } else {
    tab
  }
}
