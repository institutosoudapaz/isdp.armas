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
