gerar_id_arma <- function(tab, base) {
  if (base == "rj_complementar") {
    tab |>
      dplyr::mutate(
        id_arma = vctrs::vec_group_id(
          paste(
            id_bo,
            arma_tipo_final,
            arma_calibre_final,
            arma_marca_final,
            arma_origem,
            sep = "_"
          )
        )
      )
  }
}
