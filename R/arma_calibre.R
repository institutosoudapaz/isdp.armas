gerar_arma_calibre_final <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_calibre_final = dplyr::case_when(
        calibre_formatado_final == ".32" &
          tipo_formatado == "espingarda" ~ "32 gauge",
        calibre_formatado_final == ".32" &
          tipo_formatado %in% c("revolver", "garrucha") ~ ".32 S&W long",
        calibre_formatado_final == ".32" &
          tipo_formatado %in% c("pistola", "carabina", "submetralhadora") ~ ".32 acp",
        stringr::str_detect(calibre_formatado_final, "32$") ~ ".32 S&W long",
        TRUE ~ calibre_formatado_final
      ),
      arma_calibre_final = toupper(arma_calibre_final)
    )
}
