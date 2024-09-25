depara_tipo <- function(tab, nome_coluna = "arma_tipo") {
  tab_depara_tipo <- readxl::read_excel(
    system.file("tabelas_depara/depara_tipo.xlsx", package = "isdp.armas")
  ) |>
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
  tab_depara_calibre <- readxl::read_excel(
    system.file("tabelas_depara/depara_calibre.xlsx", package = "isdp.armas")
  ) |>
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

depara_marca <- function(tab, nome_coluna = "marca") {
  tab_depara_marca <- readxl::read_excel(
    system.file("tabelas_depara/depara_marca.xlsx", package = "isdp.armas")
  )

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
  tab_depara_crime <- readxl::read_excel(
    system.file("tabelas_depara/depara_crimes.xlsx", package = "isdp.armas")
  ) |>
    dplyr::rename_with(
      \(x) ifelse(x == "crime_original", nome_coluna, x)
    )

  tab |>
    dplyr::left_join(
      tab_depara_crime,
      by = nome_coluna
    )
}
