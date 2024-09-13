dados_armas_formatado |> 
  dplyr::select(arma_tipo, tipo_formatado) |> 
  dplyr::distinct()


dados_armas_formatado |> 
  dplyr::filter(tipo_formatado == "artesanal") |> 
  dplyr::distinct(flag_arma)


armas_final |> 
  dplyr::filter(is.na(arma_marca_final)) |> 
  dplyr::count(
    arma_marca_original,
    arma_marca_final,
    sort = TRUE
  ) |> 
  writexl::write_xlsx("~/Desktop/escape_marcas.xlsx")


armas_final |> 
  dplyr::filter(is.na(calibre_formatado_final)) |> 
  dplyr::count(
    arma_calibre_original,
    compatibilidade_tipo,
    calibre_formatado_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("~/Desktop/escape_calibre.xlsx")


dados_armas_sp_consolidado |>
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
  dplyr::count(arma_ns_terceira_letra, sort = TRUE) |> 
  print(n = 100)
