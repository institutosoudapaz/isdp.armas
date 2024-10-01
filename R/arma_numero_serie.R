gerar_sn_disponivel <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_sn_formatado = arma_numero_serie |>
        tolower() |>
        stringr::str_remove_all(" +"),
      sn_disponivel = dplyr::case_when(
        is.na(arma_sn_formatado) | arma_sn_formatado == "" ~ NA_character_,
        nchar(arma_sn_formatado) <= 4 ~ "não aparente",
        stringr::str_detect(arma_sn_formatado, "expu|supri|obl|subt|apag") ~ "removido",
        stringr::str_detect(arma_sn_formatado, "raspa|esm|lixa") ~ "raspado",
        stringr::str_detect(arma_sn_formatado, "picot|pinad") ~ "puncionamento",
        stringr::str_detect(arma_sn_formatado, "riscad|adul|sobrepost|remarcad|danific") ~ "regravado/adulterado",
        stringr::str_detect(arma_sn_formatado, "desgast|oxidad|corroi|ferru") ~ "desgastado",
        stringr::str_detect(arma_sn_formatado, "^[:punct:]+$") ~ "só pontuações",
        stringr::str_detect(arma_sn_formatado, "^[a-z/\\-]+$") ~ "só texto",
        stringr::str_detect(arma_sn_formatado, "^[0-9a-z/\\-]+$") ~ "Sim",
        TRUE ~ "não aparente"
      )
    ) |>
    dplyr::select(-arma_sn_formatado)
}

gerar_numero_serie_formatado <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_numero_serie_formatado = ifelse(
        sn_disponivel == "Sim",
        toupper(arma_numero_serie),
        ""
      ),
      arma_numero_serie_formatado = stringr::str_remove_all(
        arma_numero_serie_formatado,
        " +"
      )
    )
}
