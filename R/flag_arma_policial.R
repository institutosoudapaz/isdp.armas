gerar_flag_arma_policial <- function(tab, base) {
  if (base == "rj_complementar") {
    tab |>
      dplyr::mutate(
        flag_arma_policial = dplyr::case_when(
          tolower(patrimoniada) == "polícia militar" ~ TRUE,
          tolower(patrimoniada) == "polícia civil" ~ TRUE,
          tolower(patrimoniada) == "corpo de bombeiro" ~ TRUE,
          tolower(patrimoniada) == "força nacional" ~ TRUE,
          tolower(patrimoniada) == "forças armadas" ~ TRUE,
          tolower(patrimoniada) == "patrimoniada / outras" ~ TRUE,
          TRUE ~ FALSE
        )
      )
  }
}
