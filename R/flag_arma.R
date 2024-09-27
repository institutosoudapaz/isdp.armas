gerar_flag_arma_artesanal <- function(tab) {
  tab |>
    dplyr::mutate(
      flag_arma_artesanal = stringr::str_detect(tipo_formatado, "artesanal")
    )
}

gerar_flag_arma <- function(tab, base) {
  if (base %in% c("sp", "rj_complementar")) {
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
  } else if(base == "rj") {
    tab |>
      dplyr::mutate(
        flag_arma = as.logical(flag_arma),
        flag_arma = dplyr::case_when(
          flag_arma ~ TRUE,
          stringr::str_detect(tolower(arma_calibre), "cbc") ~ FALSE,
          arma_marca_final == "cbc" ~ FALSE,
          TRUE ~ flag_arma
        )
      )
  }
}
