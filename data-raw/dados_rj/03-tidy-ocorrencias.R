devtools::load_all()

dados_ocorrencias <- readr::read_rds("data-raw/dados_rj/dados_ocorrencias_rj.rds")
# dplyr::glimpse(dados_ocorrencias)

dados_ocorrencias_formatado <- dados_ocorrencias |> 
  dplyr::mutate(
    uf_fato = "RJ"
  )

dados_ocorrencias_consolidado <- dados_ocorrencias_formatado |>
  gerar_id_bo(base = "rj") |>
  munifacil::limpar_colunas(
    municipio_fato,
    uf_fato
  ) |> 
  munifacil::incluir_codigo_ibge() |> 
  dplyr::mutate(
    periodo_fato = categorizar_periodo(hora_fato),
    ano = lubridate::year(data_fato)
  ) |> 
  depara_crime(nome_coluna = "titulo_do") |>
  dplyr::rename(
    crime_formatado_do = crime_formatado
  ) |>
  depara_crime(nome_coluna = "titulo") |> 
  gerar_rubrica_formatada(base = "rj")

dados_ocorrencias_final <- dados_ocorrencias_consolidado |>
  dplyr::select(
    id_bo,
    controle,
    rubrica_original = titulo,
    rubrica_formatada,
    rubrica_original_do = titulo_do,
    rubrica_formatada_do,
    nome_delegacia = dp,
    data_ocorrencia_bo = data_fato,
    ano_ocorrencia_bo = ano,
    hora_ocorrencia_bo = hora_fato,
    periodo_ocorrencia_bo = periodo_fato,
    municipio = municipio_fato,
    cod_ibge = id_municipio,
    bairro = bairro_fato,
    local
  )

# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  dados_ocorrencias_final,
  glue::glue("data-raw/dados_rj/validacao/{data}_dados_ocorrencias.xlsx")
)


# Geolocalização

dados_ocorrencias_enderecos <- dados_ocorrencias_final |> 
  montar_endereco(base = "rj") |> 
  dplyr::select(id_bo, endereco)


tab_geo <- dados_ocorrencias_enderecos |> 
  dplyr::distinct(endereco) |> 
  dplyr::mutate(
    geo = geolocalizar_endereco(endereco)
  ) |> 
  tidyr::unnest(geo)

dados_ocorrencias_geolocalizadas <- dados_ocorrencias_enderecos |> 
  dplyr::left_join(
    tab_geo,
    by = "endereco"
  )

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  dados_ocorrencias_geolocalizadas,
  glue::glue("data-raw/dados_rj/validacao/{data}_dados_ocorrencias_geolocalizadas.xlsx")
)

# dados_ocorrencias_geolocalizadas |> 
#   dplyr::filter(!is.na(lat)) |>
#   dplyr::slice_sample(n = 1000) |> 
#   leaflet::leaflet() |> 
#   leaflet::addTiles() |> 
#   leaflet::addMarkers(
#     lng = ~long,
#     lat = ~lat
#   )
