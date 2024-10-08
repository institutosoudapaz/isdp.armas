dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds")

dados_ocorrencias_sp <- dados_sp |>
  dplyr::select(
    id_bo,
    id_delegacia,
    nome_departamento,
    nome_seccional,
    nome_delegacia,
    cidade,
    cod_ibge,
    num_bo,
    nome_departamento_circ,
    nome_seccional_circ,
    nome_delegacia_circ,
    nome_municipio_circ,
    cod_ibge_circ,
    descr_tipo_bo,
    data_ocorrencia_bo,
    ano_bo,
    ano_bo_novo,
    mes_bo,
    hora_ocorrencia_bo,
    periodo_ocorrencia_bo,
    descricao_apresentacao,
    datahora_registro_bo,
    data_comunicacao_bo,
    autoria_bo,
    flag_ato_infracional,
    flag_flagrante,
    flag_status,
    rubrica,
    descr_conduta,
    desdobramento,
    bairro,
    descr_tipolocal,
    descr_subtipolocal,
    cep,
    logradouro,
    numero_logradouro,
    latitude,
    longitude,
    flag_vitima_violencia_domestica,
    cont_pessoa,
    cont_arma
  )

tabela_ref = munifacil::depara_muni_codigo() |>
  dplyr::bind_rows(
    tibble::tibble(
      muni_join = c("sjose da bela vista", "ribeirao indios"),
      uf_join = "SP",
      id_municipio = c('3549508', '3543238'),
      manual = TRUE,
      existia_1991 = c(TRUE, FALSE),
      existia_2000 = TRUE,
      existia_2010 = TRUE
    )
  )

locations <- data <- data.frame(
  DE = c(
    "Via pública",
    "Residência",
    "Unidade rural",
    "Condominio Residencial",
    "Comércio e Serviços",
    "Condomínio Residencial",
    "Restaurante e Afins",
    "Centro Comerc./Empresarial",
    "Rodovia/Estrada",
    "Favela",
    "(blank)",
    "Area não Ocupada",
    "Serviços e Bens Públicos",
    "Shopping Center",
    "Estabelecimento de ensino",
    "Repartição Pública",
    "Condominio Comercial",
    "Saúde",
    "Condomínio Comercial",
    "Local clandestino/ilegal",
    "Terminal/Estação",
    "Hospedagem",
    "Centro Comercial/Empresarial",
    "Estabelecimento Industrial",
    "Escritório",
    "Lazer e Recreação",
    "Estabelecimento prisional",
    "Estabelecimento Bancário",
    "NULL",
    "Estacionamento/Garagem",
    "Veículo em movimento",
    "Estrada de ferro",
    "Entidade Assistencial",
    "Templo e afins",
    "Carro Forte",
    "Garagem coletiva de prédio",
    "Garagem ou abrigo de residência",
    "Estacionamento particular",
    "Estacionamento com vigilância",
    "Estacionamento público"
  ),
  PARA = c(
    "Via pública",
    "Residência",
    "Unidade rural",
    "Residência",
    "Comércio e Serviços",
    "Residência",
    "LAZER/HOSPEDAGEM",
    "Comércio e Serviços",
    "Rodovia/Ferrovia",
    "Via pública",
    "NA",
    "Via pública",
    "Repartição Pública",
    "Comércio e Serviços",
    "Estabelecimento de ensino",
    "Repartição Pública",
    "Comércio e Serviços",
    "Repartição Pública",
    "Comércio e Serviços",
    "Via pública",
    "Via pública",
    "LAZER/HOSPEDAGEM",
    "Comércio e Serviços",
    "Comércio e Serviços",
    "Comércio e Serviços",
    "LAZER/HOSPEDAGEM",
    "Repartição Pública",
    "Instituição bancária / Carro forte",
    "NA",
    "Estacionamento",
    "Veículo",
    "Rodovia/Ferrovia",
    "Comércio e Serviços",
    "Comércio e Serviços",
    "Instituição bancária / Carro forte",
    "Estacionamento",
    "Estacionamento",
    "Estacionamento",
    "Estacionamento",
    "Estacionamento"
  )
) |>
  dplyr::mutate(
    DE = DE |>
      stringr::str_to_lower() |>
      abjutils::rm_accent()
  ) |>
  dplyr::distinct(.keep_all = TRUE)

depara_rubricas <- readxl::read_excel("data-raw/depara_rubricas.xlsx") |>
  janitor::clean_names()

dados_ocorrencias_sp_tidy <- dados_ocorrencias_sp |>
  dplyr::mutate(
    muni= dplyr::coalesce(cidade, nome_municipio_circ),
    uf = "SP"
  ) |>
  munifacil::limpar_colunas(muni, uf) |>
  munifacil::incluir_codigo_ibge(
    tabela_ref
  ) |>
  dplyr::mutate(
    ano_bo_novo = lubridate::year(data_ocorrencia_bo),
    mes_bo = lubridate::month(data_ocorrencia_bo),
    periodo_ocorrencia = dplyr::case_when(
      hora_ocorrencia_bo <= "04:00" ~ "Madrugada",
      hora_ocorrencia_bo <= "08:00" ~ "Início Manhã",
      hora_ocorrencia_bo <= "12:00" ~ "Manhã",
      hora_ocorrencia_bo <= "16:00" ~ "Início Tarde",
      hora_ocorrencia_bo <= "20:00" ~ "Tarde",
      hora_ocorrencia_bo > "20:00" ~ "Noite",
      TRUE ~ NA_character_
    ),
    DE = descr_tipolocal |>
      stringr::str_to_lower() |>
      abjutils::rm_accent()) |>
  dplyr::left_join(locations) |>
  dplyr::left_join(depara_rubricas) |>
  dplyr::mutate(
    rubrica2 = dplyr::case_when(
      is.na(rubrica2) & stringr::str_detect(rubrica, "Homicídio") ~ "Homicídio",
      TRUE ~ rubrica2
    )
  ) |>
  dplyr::distinct(id_bo, .keep_all = TRUE) |>
  dplyr::select(
    id_bo,
    id_delegacia,
    nome_departamento,
    nome_seccional,
    nome_delegacia,
    cidade,
    municipio = nome_municipio_circ,
    id_municipio_ibge = id_municipio,
    manual,
    ano_bo,
    num_bo,
    nome_departamento_circ,
    nome_seccional_circ,
    nome_delegacia_circ,
    nome_municipio_circ,
    descr_tipo_bo,
    data_ocorrencia_bo,
    ano_bo_formatado = ano_bo_novo,
    mes_bo,
    hora_ocorrencia_bo,
    periodo_ocorrencia,
    condutor = descricao_apresentacao,
    datahora_registro_bo,
    data_comunicacao_bo,
    autoria_bo,
    tentado_consumado = flag_status,
    rubrica_original = rubrica,
    descr_conduta,
    desdobramento,
    rubrica_formtada = rubrica2,
    desdobramento,
    bairro,
    tipo_local_original = descr_tipolocal,
    descr_subtipolocal,
    tipo_local_formatado = PARA,
    cep,
    logradouro,
    numero_logradouro,
    latitude,
    longitude,
    flag_vitima_violencia_domestica,
    flag_ato_infracional
  ) |>
  dplyr::left_join(
    dplyr::group_by(armas_final, id_bo) |>
      dplyr::summarise(cont_arma = n())
  )

base_enderecos <- dados_ocorrencias_sp_tidy |>
  dplyr::filter((is.na(latitude) | latitude == 0), (!is.na(logradouro) | !is.na(cep))) |>
  dplyr::distinct(logradouro, numero_logradouro, cidade, cep)

#enderecos <- paste(resuminho$`Nome do Logradouro`, resuminho$Número)

# enderecos_postalcode <- tidygeocoder::geo(
#   country = rep("Brazil", nrow(base_enderecos)),
#   postalcode = base_enderecos$cep,
#   method = "osm")
#
# enderecos_logradouro <- tidygeocoder::geo(
#   country = rep("Brazil", nrow(base_enderecos)),
#   city = base_enderecos$cidade,
#   state = rep("São Paulo", nrow(base_enderecos)),
#   street = paste(base_enderecos$logradouro, base_enderecos$numero_logradouro),
#   method = "osm")

# dados_ocorrencias_sp_tidy |>
#   filter(
#     (latitude == 0 |is.na(latitude)),
#     (!is.na(logradouro) | !is.na(cep))) |>
#   View()
#
# dados_ocorrencias_sp_tidy |> View()

devtools::install_github("curso-r/munifacil")

dados_ocorrencias_sp_tidy |>
  left_join(
    bind_cols(
      base_enderecos,
      enderecos_postalcode |> select(latitude_cep = lat, longitude_cep = long)
    )
  ) |>
  left_join(
    bind_cols(
      base_enderecos,
      enderecos_logradouro |> select(latitude_logr = lat, longitude_logr = long)
    )
  ) |>
  mutate(
    latitude_final = coalesce(latitude, latitude_logr, latitude_cep),
    longitude_final = coalesce(longitude, longitude_logr, longitude_cep)
  ) |>
  writexl::write_xlsx("inst/dados_sp/20240913_dados_ocorrencias_sem_lat_lon.xlsx")

latlons_completos <- readRDS("inst/dados_sp/dados_ocorrencias_sp_latlon.rds") |>
  select(id_bo, latitude_cep, longitude_cep, latitude_logr, longitude_logr)

dados_ocorrencias_sp_tidy |>
  # left_join(
  #   bind_cols(
  #     base_enderecos,
  #     enderecos_postalcode |> select(latitude_cep = lat, longitude_cep = long)
  #   )
  # ) |>
  # left_join(
  #   bind_cols(
  #     base_enderecos,
  #     enderecos_logradouro |> select(latitude_logr = lat, longitude_logr = long)
  #   )
  # ) |>
  left_join(latlons_completos) |>
  mutate(
    latitude_final = coalesce(latitude, latitude_logr, latitude_cep),
    longitude_final = coalesce(longitude, longitude_logr, longitude_cep)
  ) |>
  readr::write_rds("inst/dados_sp/dados_ocorrencias_sp.rds", compress = "xz")

dados_ocorrencias_sp_tidy |>
  # left_join(
  #   bind_cols(
  #     base_enderecos,
  #     enderecos_postalcode |> select(latitude_cep = lat, longitude_cep = long)
  #   )
  # ) |>
  # left_join(
  #   bind_cols(
  #     base_enderecos,
  #     enderecos_logradouro |> select(latitude_logr = lat, longitude_logr = long)
  #   )
  # ) |>
  left_join(latlons_completos) |>
  mutate(
    latitude_final = coalesce(latitude, latitude_logr, latitude_cep),
    longitude_final = coalesce(longitude, longitude_logr, longitude_cep)
  ) |>
  writexl::write_xlsx("inst/dados_sp/20240930_ocorrencias.xlsx")
