dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds")

# Número por ano

dados_sp |> 
  dplyr::distinct(id_bo, ano_bo) |> 
  dplyr::count(ano_bo)


# Número de série

dados_sp |> 
  dplyr::filter(!stringr::str_detect(numero_arma, "[0-9]")) |> 
  dplyr::mutate(
    arma_numero_serie = toupper(numero_arma)
  ) |> 
  dplyr::count(numero_arma, flag_arma_fogo, sort = TRUE) |> 
  writexl::write_xlsx("data-raw/dados_sp/docs/validacao_numero_serie.xlsx")


# Endereços diferentes
dados_sp |> 
  dplyr::group_by(id_bo) |> 
  dplyr::summarise(
    n = dplyr::n_distinct(logradouro)
  ) |> 
  dplyr::arrange(desc(n)) |> 
  dplyr::filter(n > 1)


# dados_sp |> 
#   dplyr::group_by(id_bo) |> 
#   dplyr::select(dplyr::where(~ length(unique(.x)) > 1))