gerar_flag_arma_artesanal <- function(tab) {
  tab |>
    dplyr::mutate(
      flag_arma_artesanal = stringr::str_detect(tipo_formatado, "artesanal")
    )
}

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
