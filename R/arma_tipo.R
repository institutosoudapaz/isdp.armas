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

gerar_tipo_arma_final <- function(tab) {
  tab |>
    dplyr::rename(
      compatibilidade_tipo = tipo_arma_calibre
    ) |> 
    dplyr::mutate(
      tipo_formatado = tolower(tipo_formatado),
      tipo_formatado = stringr::str_squish(tipo_formatado)
    )
}