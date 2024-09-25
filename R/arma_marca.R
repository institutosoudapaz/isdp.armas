gerar_arma_marca_final <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_marca_final = dplyr::case_when(
        stringr::str_detect(tolower(arma_marca), "taur") ~ "taurus",
        stringr::str_detect(tolower(arma_marca), "ross") ~ "rossi",
        stringr::str_detect(tolower(arma_marca), "glo[ck]") ~ "glock",
        stringr::str_detect(tolower(arma_marca), "smith") ~ "s&w",
        TRUE ~ tolower(marca_arma_v2)
      )
    )
}
