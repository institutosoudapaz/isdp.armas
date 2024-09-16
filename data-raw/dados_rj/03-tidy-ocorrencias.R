devtools::load_all()

dados_ocorrencias <- readr::read_rds("inst/dados_rj/dados_ocorrencias_rj.rds")
# dplyr::glimpse(dados_ocorrencias)

dados_ocorrencias_formatado <- dados_ocorrencias |> 
  dplyr::select(
    id_bo = controle,
    ano_bo = ano,
    mes_bo = mes,
    rubrica = titulo,
    delegacia_nome = dp,
    cisp,
    aisp,
    risp,
    data_com,
    data_fato,
    hora_fato,
    municipio_fato,
    bairro_fato,
    local_fato = local
  )

dados_ocorrencias_final <- dados_ocorrencias_formatado

# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  dados_ocorrencias_final,
  glue::glue("inst/dados_rj/{data}_dados_ocorrencias.xlsx")
)
