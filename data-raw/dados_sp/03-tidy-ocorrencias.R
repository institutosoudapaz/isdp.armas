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

dados_ocorrencias_sp |>
  readr::write_rds("inst/dados_sp/dados_ocorrencias_sp.rds", compress = "xz")
