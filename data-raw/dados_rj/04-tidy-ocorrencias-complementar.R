devtools::load_all()

dados_armas_complementar <- readr::read_rds("data-raw/dados_rj/dados_armas_complementar.rds")

dados_ocorrencias_formatado <- dados_armas_complementar |>
  dplyr::select(
    id_bo = controle_interno_sco,
    circunscricao,
    numero_procedimento,
    bairro,
    data_registro,
    acautelamento,
    tipo_delito
  ) |>
  tidyr::separate_longer_delim(
    col = tipo_delito,
    delim = "///"
  ) |>
  depara_crime("tipo_delito")

dados_ocorrencias_final <- dados_ocorrencias_formatado

# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  dados_ocorrencias_final,
  glue::glue("data-raw/dados_rj/validacao/{data}_dados_ocorrencias_complementar.xlsx")
)

