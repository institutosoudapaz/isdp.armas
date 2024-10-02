gerar_id_bo <- function(tab, base) {
  if (base == "rj_complementar") {
    tab |>
      dplyr::mutate(
        id_bo = vctrs::vec_group_id(controle_interno_sco)
      )
  } else if (base == "rj") {
    tab |>
      dplyr::mutate(
        id_bo = vctrs::vec_group_id(controle)
      )
  } else {
    tab
  }
}
