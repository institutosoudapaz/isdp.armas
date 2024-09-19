devtools::load_all()

dados_ocorrencias <- readr::read_rds("inst/dados_rj/dados_ocorrencias_rj.rds")
# dplyr::glimpse(dados_ocorrencias)

dados_ocorrencias_formatado <- dados_ocorrencias |> 
  dplyr::mutate(
    uf_fato = "RJ"
  ) |> 
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
    uf_fato,
    municipio_fato,
    bairro_fato,
    local_fato = local
  )

dados_ocorrencias_final <- dados_ocorrencias_formatado |> 
  munifacil::limpar_colunas(
    municipio_fato,
    uf_fato
  ) |> 
  munifacil::incluir_codigo_ibge() |> 
  dplyr::mutate(
    periodo_fato = categorizar_periodo(hora_fato)
  ) |> 
  dplyr::select(
    id_bo,
    ano_bo,
    mes_bo,
    rubrica,
    delegacia_nome,
    cisp,
    aisp,
    risp,
    data_com,
    data_fato,
    hora_fato,
    periodo_fato,
    municipio_fato,
    cod_ibge = id_municipio,
    bairro_fato,
    local_fato
  )

# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  dados_ocorrencias_final,
  glue::glue("inst/dados_rj/{data}_dados_ocorrencias.xlsx")
)


# Geolocalização

dados_ocorrencias_enderecos <- dados_ocorrencias_final |> 
  montar_endereco_rj() |> 
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
  glue::glue("inst/dados_rj/{data}_dados_ocorrencias_geolocalizadas.xlsx")
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
