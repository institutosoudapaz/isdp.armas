ler_depara <- function(aba) {
  readxl::read_excel(
    system.file(
      "tabelas_depara/gabarito_correcoes_pacote_armas.xlsx",
      package = "isdp.armas"
    ),
    sheet = aba
  )
}

depara_tipo <- function(tab, nome_coluna = "arma_tipo") {
  tab_depara_tipo <- ler_depara("tipo") |>
    dplyr::distinct(tipo, .keep_all = TRUE)

  if (nome_coluna != "tipo") {
    tab_depara_tipo <- tab_depara_tipo |>
      dplyr::rename_with(
        \(x) ifelse(x == "tipo", nome_coluna, x)
      )
  }

  tab |>
    dplyr::left_join(
      tab_depara_tipo,
      by = nome_coluna
    )
}

depara_calibre <- function(tab, nome_coluna = "calibre") {
  tab_depara_calibre <- ler_depara("calibre") |>
    dplyr::distinct(calibre, .keep_all = TRUE)

  if (nome_coluna != "calibre") {
    tab_depara_calibre <- tab_depara_calibre |>
      dplyr::rename_with(
        \(x) ifelse(x == "calibre", nome_coluna, x)
      )
  }

  tab |>
    dplyr::left_join(
      tab_depara_calibre,
      by = nome_coluna
    )
}

depara_calibre_policial <- function(tab) {
  tab_depara_calibre <- ler_depara("calibre_arma_policial") |>
    dplyr::distinct(arma_calibre_final, tipo_formatado, .keep_all = TRUE)

  tab |>
    dplyr::left_join(
      tab_depara_calibre,
      by = c("arma_calibre_final", "tipo_formatado")
    )
}

depara_marca <- function(tab, nome_coluna = "marca") {
  tab_depara_marca <- ler_depara("marca")

  if (nome_coluna != "marca") {
    tab_depara_marca <- tab_depara_marca |>
      dplyr::rename_with(
        \(x) ifelse(x == "marca", nome_coluna, x)
      )
  }

  tab |>
    dplyr::left_join(
      tab_depara_marca,
      by = nome_coluna
    )
}

depara_crime <- function(tab, nome_coluna) {
  tab_depara_crime <- ler_depara("crimes") |>
    dplyr::rename_with(
      \(x) ifelse(x == "crime_original", nome_coluna, x)
    )

  tab |>
    dplyr::left_join(
      tab_depara_crime,
      by = nome_coluna
    )
}
