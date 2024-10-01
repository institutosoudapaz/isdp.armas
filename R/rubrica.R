gerar_rubrica_formatada <- function(tab) {
  tab |> 
    depara_crime(
      nome_coluna = "tipo_delito"
    ) |> 
    dplyr::rename(
      rubrica_formatada = crime_formatado
    )
}