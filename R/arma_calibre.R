gerar_arma_calibre_final <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_calibre_final = dplyr::case_when(
        calibre_formatado_final == ".32 ou 34 G" &
          arma_tipo_final == "espingarda" ~ "32 gauge",
        calibre_formatado_final == ".32 ou 34 G" &
          arma_tipo_final %in% c("revolver", "garrucha") ~ ".32 S&W long",
        calibre_formatado_final == ".32 ou 34 G" &
          arma_tipo_final %in% c("pistola", "carabina", "submetralhadora") ~ ".32 acp",
        stringr::str_detect(calibre_formatado_final, "32$") ~ ".32 S&W long",
        calibre_formatado_final == ".32 ou 34 G" ~ ".32",
        TRUE ~ calibre_formatado_final
      ),
      arma_calibre_final = toupper(arma_calibre_final)
    )
}
