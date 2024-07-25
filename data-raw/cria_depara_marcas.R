calibre_isdp <- readxl::read_excel(sheet = 1, "data-raw/depara_marcas_bruto.xlsx") |>
  janitor::clean_names() |>
  dplyr::select(-nome_completo_uso_interno) |>
  dplyr::mutate(
    arma_marca = purrr::map(marca_arma, stringr::str_split, pattern = ",")
  ) |>
  tidyr::unnest(arma_marca) |>
  tidyr::unnest(arma_marca) |>
  dplyr::mutate(
    arma_marca = tolower(stringr::str_squish(arma_marca))
  ) |>
  dplyr::distinct(arma_marca, .keep_all = TRUE)

depara_marca_final <- readxl::read_excel("data-raw/depara_marca_legado.xlsx") |>
  dplyr::transmute(
    arma_marca = tolower(marca),
    marca_arma_v2 = toupper(marca_formatada)
  ) |>
  dplyr::bind_rows(calibre_isdp) |>
  dplyr::group_by(marca_arma_v2) |>
  dplyr::mutate(
    pais_fabricacao = dplyr::last(sort(unique(pais_fabricacao)))
  ) |>
  dplyr::distinct(arma_marca, .keep_all = TRUE)

writexl::write_xlsx(depara_marca_final, "inst/tabelas_depara/depara_marca.xlsx")
