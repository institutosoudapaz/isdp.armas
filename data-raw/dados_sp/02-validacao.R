dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds")

# Número por ano

dados_sp |> 
  dplyr::distinct(id_bo, ano_bo) |> 
  dplyr::count(ano_bo)
