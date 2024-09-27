gerar_id_arma <- function(tab, base) {
  if (base %in% c("rj", "rj_complementar")) {
    tab |>
      dplyr::mutate(
        id_arma = vctrs::vec_group_id(
          paste(
            id_bo,
            tipo_formatado,
            arma_calibre_final,
            arma_marca_final,
            arma_origem,
            sep = "_"
          )
        )
      )
  } else if (base == "sp") {
    tab |> 
      dplyr::mutate(
        id_arma = vctrs::vec_group_id(paste0(id_bo, "_", cont_arma))
      )
  }
}
