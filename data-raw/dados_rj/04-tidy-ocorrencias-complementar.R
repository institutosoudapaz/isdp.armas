devtools::load_all()

dados_armas_complementar <- readr::read_rds("inst/dados_rj/dados_armas_complementar.rds")

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

armas_final <- dados_ocorrencias_formatado

# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  armas_final,
  glue::glue("inst/dados_rj/{data}_dados_ocorrencias_complementar.xlsx")
)

