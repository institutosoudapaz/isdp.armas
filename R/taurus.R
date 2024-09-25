aplicar_regra_1 <- function(tab, calibre) {
  depara_regra_1 <- ler_depara("regra_taurus_1")

  tab |>
    dplyr::filter(
      sn_disponivel == "Sim",
      !is.na(arma_numero_serie),
      !stringr::str_detect(arma_numero_serie, "[A-Za-z\\-/]"),
      arma_marca_final == "taurus",
      arma_tipo_final == "revolver"
    ) |>
    dplyr::mutate(
      arma_numero_serie = as.numeric(arma_numero_serie),
      arma_calibre = ifelse(
        stringr::str_detect(arma_calibre_final, "[.](32|22|38) "),
        stringr::str_sub(arma_calibre_final, 1, 3),
        arma_calibre_final
      )
    ) |>
    dplyr::left_join(
      depara_regra_1,
      by = dplyr::join_by(
        "arma_calibre",
        dplyr::between("arma_numero_serie", "num_serie_min", "num_serie_max")
      )
    ) |>
    dplyr::select(
      id_bo,
      id_arma,
      arma_ano_fabricacao
    ) |>
    dplyr::distinct(id_bo, id_arma, .keep_all = TRUE) |>
    dplyr::mutate(
      padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 1")
    )
}


aplicar_regra_2_1 <- function(tab) {
  depara_regra_2 <- ler_depara("regra_taurus_2")

  tab |>
    dplyr::filter(
      sn_disponivel == "Sim",
      !is.na(arma_numero_serie),
      nchar(arma_numero_serie) == 8,
      stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
      arma_marca_final == "taurus",
      arma_tipo_final == "pistola"
    ) |>
    dplyr::mutate(
      arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie, 2, 2),
      arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie, 3, 3)
    ) |>
    dplyr::left_join(
      depara_regra_2,
      by = dplyr::join_by(
        "arma_ns_primeira_letra",
        "arma_ns_segunda_letra"
      )
    ) |>
    dplyr::select(
      id_bo,
      id_arma,
      arma_ano_fabricacao
    ) |>
    dplyr::mutate(
      padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 2.1")
    )
}

aplicar_regra_2_2 <- function(tab) {
  depara_regra_2 <- ler_depara("regra_taurus_3")

  tab |>
    dplyr::filter(
      sn_disponivel == "Sim",
      !is.na(arma_numero_serie),
      nchar(arma_numero_serie) %in% c(7, 8),
      stringr::str_detect(arma_numero_serie, "^[A-Z]{2}[0-9]"),
      arma_marca_final == "taurus",
      arma_tipo_final == "revolver"
    ) |>
    dplyr::mutate(
      arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie, 1, 1),
      arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie, 2, 2)
    ) |>
    dplyr::left_join(
      depara_regra_2,
      by = dplyr::join_by(
        "arma_ns_primeira_letra",
        "arma_ns_segunda_letra"
      )
    ) |>
    dplyr::select(
      id_bo,
      id_arma,
      arma_ano_fabricacao
    ) |>
    dplyr::mutate(
      padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 2.2")
    )
}

aplicar_regra_3 <- function(tab) {
  depara_regra_3 <- readxl::read_excel(
    system.file(
      "tabelas_depara/taurus_regra_3.xlsx",
      package = "isdp.armas"
    )
  )

  dados_armas_consolidado |>
    dplyr::filter(
      !is.na(arma_numero_serie),
      nchar(arma_numero_serie) == 9,
      stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
      arma_marca_final == "taurus"
    ) |>
    dplyr::mutate(
      arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie, 1, 1),
      arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie, 2, 2),
      arma_ns_terceira_letra = stringr::str_sub(arma_numero_serie, 3, 3)
    ) |>
    dplyr::left_join(
      depara_regra_3,
      by = dplyr::join_by(
        "arma_ns_primeira_letra",
        "arma_ns_segunda_letra",
        "arma_ns_terceira_letra"
      )
    ) |>
    dplyr::select(
      id_bo,
      id_arma,
      arma_ano_fabricacao
    ) |>
    dplyr::mutate(
      padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 3")
    )

}
