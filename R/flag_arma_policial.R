gerar_flag_arma_policia_prop <- function(tab, base) {
  if (base == "rj_complementar") {
    tab |>
      dplyr::mutate(
        flag_arma_policia_prop = dplyr::case_when(
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

gerar_flag_mdoip <- function(tab) {
  tab_mdoip <- ler_depara("crimes") |>
    dplyr::filter(crime_formatado == "MDOIP")

  tab |>
    dplyr::mutate(
      flag_mdoip = purrr::map_lgl(
        tipo_delito,
        \(x) any(stringr::str_detect(x, tab_mdoip$crime_original))
      )
    )
}

gerar_flag_arma_policial <- function(tab, base) {
  if (base == "rj_complementar") {
    tab |>
      gerar_flag_arma_policia_prop(base = "rj_complementar") |> 
      gerar_flag_mdoip() |> 
      depara_calibre_policial() |> 
      dplyr::mutate(
        flag_arma_policial = dplyr::case_when(
          sn_disponivel != "Sim" ~ FALSE,
          flag_arma_policia_prop & flag_mdoip ~ TRUE,
          flag_calibre_policial & flag_mdoip ~ TRUE,
          TRUE ~ FALSE
        )
      )
  }
}
