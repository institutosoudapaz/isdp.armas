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
    dplyr::mutate(
      arma_tipo_final = dplyr::case_when(
        tipo_formatado == "artesanal" ~ "artesanal",
        !flag_tipo_arma_incompativel_calibre ~ tipo_formatado,
        !is.na(tipo_arma_calibre) ~ tipo_arma_calibre,
        TRUE ~ tipo_formatado
      ),
      arma_tipo_final = tolower(arma_tipo_final),
      arma_tipo_final = stringr::str_squish(arma_tipo_final)
    )
}