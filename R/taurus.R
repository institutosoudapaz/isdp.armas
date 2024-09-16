aplicar_regra_1 <- function(tab, calibre) {
  depara_regra_1 <- readxl::read_excel(
    system.file(
      "tabelas_depara/taurus_regra_1.xlsx",
      package = "isdp.armas"
    ),
    col_types = "text"
  ) |>
    dplyr::rename(
      arma_calibre_join = arma_calibre
    )

  tab |>
    dplyr::left_join(
      depara_regra_1,
      by = dplyr::join_by(
        "arma_calibre_join",
        dplyr::between("arma_numero_serie", "num_serie_min", "num_serie_max")
      )
    )
}


aplicar_regra_2 <- function(tab) {
  depara_regra_2 <- readxl::read_excel(
    system.file(
      "tabelas_depara/taurus_regra_2.xlsx",
      package = "isdp.armas"
    )
  )

  tab |>
    dplyr::left_join(
      depara_regra_2,
      by = dplyr::join_by(
        "arma_ns_primeira_letra",
        "arma_ns_segunda_letra"
      )
    )
}

aplicar_regra_3 <- function(tab) {
  depara_regra_3 <- readxl::read_excel(
    system.file(
      "tabelas_depara/taurus_regra_3.xlsx",
      package = "isdp.armas"
    )
  )

  tab |>
    dplyr::left_join(
      depara_regra_3,
      by = dplyr::join_by(
        "arma_ns_primeira_letra",
        "arma_ns_segunda_letra",
        "arma_ns_terceira_letra"
      )
    )
}
