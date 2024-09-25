gerar_flag_arma_artesanal <- function(tab) {
  tab |>
    dplyr::mutate(
      flag_arma_artesanal = stringr::str_detect(arma_tipo_final, "artesanal")
    )
}

gerar_flag_arma <- function(tab) {
  tab |>
    dplyr::mutate(
      flag_arma = as.logical(flag_arma),
      flag_arma = dplyr::case_when(
        flag_arma ~ TRUE,
        stringr::str_detect(tolower(arma_numero_serie), "simulacro|muni[çc][ãa]o|chumbo") ~ FALSE,
        stringr::str_detect(tolower(arma_calibre), "cbc") ~ FALSE,
        arma_marca_final == "cbc" ~ FALSE,
        TRUE ~ flag_arma
      )
    )
}
