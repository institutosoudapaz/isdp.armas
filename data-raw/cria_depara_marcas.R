dados <- readxl::read_excel('data-raw/tabelas_depara/depara_marcas_bruto.xlsx')
marca_legado <- readxl::read_excel("data-raw/tabelas_depara/depara_marca_legado.xlsx")


final <- dados |>
  dplyr::mutate(
    MARCA_ARMA = purrr::map(MARCA_ARMA, function(x){
      tibble::tibble(MARCA_ARMA = as.vector(stringr::str_split(x, pattern = ", ?", simplify = TRUE)))
    })
  ) |>
  tidyr::unnest(MARCA_ARMA) |>
  dplyr::rename(arma_marca = MARCA_ARMA, marca_arma_v2 = MARCA_ARMA_V2,
         pais_fabricacao = PAIS_FABRICACAO) |>
  dplyr::bind_rows(marca_legado) |>
  dplyr::distinct(arma_marca, marca_arma_v2, pais_fabricacao) |>
  dplyr::arrange(marca_arma_v2) |>
  janitor::clean_names() |>
  dplyr::filter(arma_marca != "") |>
  dplyr::mutate(
    marca_arma_v2 = dplyr::case_when(
      marca_arma_v2 == "SMITH & WESSON" ~ "S&W",
      marca_arma_v2 == "KEL-TEC" ~ "KEL TEC",
      TRUE ~ toupper(marca_arma_v2))
  ) |>
  dplyr::distinct(arma_marca, marca_arma_v2, .keep_all = TRUE)

final |>
  writexl::write_xlsx("inst/tabelas_depara/depara_marca.xlsx")
